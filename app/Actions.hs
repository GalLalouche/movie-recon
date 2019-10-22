{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Actions(
  APIAndDB,
  updateMoviesForAllFollowedPersons,
  addFollowedPerson,
  parseSeenMovies,
  getFormattedUnseenMovies,
) where

import           Data.String.Interpolate          (i)
import           Data.Text                        (pack, splitOn, unpack)

import           MovieDB.API                      (ApiKey, personCredits, personName)
import           MovieDB.Database.Common          (DbCall, DbPath)
import qualified MovieDB.Database.FilteredMovies  as FM
import qualified MovieDB.Database.FollowedPersons as FP
import qualified MovieDB.Database.Movies          as M
import qualified MovieDB.Database.Participations  as P
import           MovieDB.Parsers                  (Url(..), parseId)
import           MovieDB.Types                    (FilteredMovie(..), Movie(..), MovieId(..), Participation(..), Person(..), mkMovieId)
import qualified MovieDB.Types                    as Types

import           Control.Monad                    ((>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT, withReaderT)
import           Data.Foldable                    (traverse_)
import           Data.Functor                     (void)

import           Common.Maybes                    (orError)
import           Common.MonadPluses               (traverseFilter)
import           Common.Traversables              (traverseFproduct)


type APIAndDB = ReaderT (ApiKey, DbPath) IO ()
getUnseenMovies :: DbCall [Movie]
getUnseenMovies = M.allMovies >>= traverseFilter FM.isNotFiltered

getFollowedParticipations :: Movie -> DbCall [Participation]
getFollowedParticipations = P.getParticipationsForMovie >=> traverseFilter (FP.isFollowed . Types.person)

withDbPath = withReaderT snd
withKey = withReaderT fst

updateMoviesForAllFollowedPersons :: APIAndDB
updateMoviesForAllFollowedPersons = do
  followedPersons <- withDbPath FP.allFollowedPersons
  participations <- withKey $ concat <$> traverse personCredits followedPersons
  void $ withDbPath $ traverse P.addValueEntry participations

addFollowedPerson :: String -> APIAndDB
addFollowedPerson url = do
  let id = parseId $ Url $ pack url
  name <- withKey $ personName id
  _ <- liftIO $ putStrLn [i|Adding <#{name}> and their credits...|]
  let person = Person {_id = id, _name = name}
  _ <- withDbPath $ FP.addFollowedPerson person
  participations <- withKey $ personCredits person
  traverse_ (withDbPath . P.addValueEntry) participations

--TODO parse from stdin, not from a file
parseSeenMovies :: FilePath -> DbCall ()
parseSeenMovies f = do
  ls <- lines <$> liftIO (readFile f)
  movies <- traverse parse ls
  traverse_ FM.addFilteredMovie movies
  where
    parse :: String -> DbCall FilteredMovie
    parse line = do
      let r : id = unpack $ head $ splitOn "\t" $ pack line
      let reason | r == 'S' = Types.Seen | r == 'I' = Types.Ignored | otherwise = error [i|Unsupported prefix <#{r}>|]
      movie <- getValueOrError $ mkMovieId $ pack id
      return $ FilteredMovie movie reason
    getValueOrError :: MovieId -> DbCall Movie
    getValueOrError mid = orError [i|Could not find movie with ID <#{mid}>|] <$> runMaybeT (M.getValue mid)

mkStringMovie (Movie (MovieId id) name date) = [i|#{id}\t#{name}\t#{date}|]

mkStringMovieAndParticipations :: Movie -> [Participation] -> String
mkStringMovieAndParticipations m ps = let
    movieString = mkStringMovie m
    participationStrings = map mkStringParticipation ps
  in unlines $ movieString : participationStrings where
  mkStringParticipation (Participation p _ pt) = let
      maybeRole = case pt of
        Types.Actor -> ""
        e           -> [i| (#{e})|]
      name = Types._name (p :: Person)
    in [i|\t#{name}#{maybeRole}|]

getFormattedUnseenMovies :: Bool -> DbCall ()
getFormattedUnseenMovies verbose = do
  movies <- Actions.getUnseenMovies
  participations <- traverseFproduct Actions.getFollowedParticipations movies
  let formattedMovies = map mkStringMovie movies
  let formattedParticipations = map (uncurry mkStringMovieAndParticipations) participations
  liftIO $ putStr $ unlines $ if verbose then formattedParticipations else formattedMovies
