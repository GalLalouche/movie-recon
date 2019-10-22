{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

import           Data.String.Interpolate          (i)
import           Data.Text                        (pack, splitOn, unpack)

import           MovieDB.API                      (ApiKey(..), personCredits, personName, readKey)
import           MovieDB.Database.Common          (DbPath(..))
import qualified MovieDB.Database.FilteredMovies  as FM
import qualified MovieDB.Database.FollowedPersons as FP
import qualified MovieDB.Database.Movies          as M
import qualified MovieDB.Database.Participations  as P
import           MovieDB.Parsers                  (Url(..), parseId)
import           MovieDB.Types                    (Movie(..), MovieId(..), Participation(..), Person(..), FilteredMovie(..), mkMovieId)
import qualified MovieDB.Types                    as Types

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT)
import           Data.Foldable                    (traverse_)
import           Data.Functor                     (void)

import qualified Config

import           Common.Maybes                    (orError)
import           Common.MonadPluses               (traverseFilter)
import           Common.Operators
import           Common.Traversables              (traverseFproduct)

dbPath = DbPath "db.sqlite"

withKey :: ReaderT ApiKey IO a -> IO a
withKey r = liftIO readKey >>= runReaderT r

withDbPath :: Monad m => ReaderT DbPath m a -> m a
withDbPath = flip runReaderT dbPath

updateMoviesForAllFollowedPersons :: IO ()
updateMoviesForAllFollowedPersons = do
  followedPersons <- withDbPath FP.allFollowedPersons
  participations <- withKey $ concat <$> traverse personCredits followedPersons
  void $ withDbPath $ traverse P.addValueEntry participations

getUnseenMovies :: IO [Movie]
getUnseenMovies = do
  movies <- withDbPath M.allMovies :: IO [Movie]
  traverseFilter (withDbPath . FM.isNotFiltered) movies :: IO [Movie]

getFollowedParticipations :: Movie -> IO [Participation]
getFollowedParticipations m = do
  ps <- withDbPath $ P.getParticipationsForMovie m
  traverseFilter isFollowed ps where
    isFollowed :: Participation -> IO Bool
    isFollowed = withDbPath . FP.isFollowed . person

mkStringMovie (Movie (MovieId id) name date) = [i|#{id}\t#{name}\t#{date}|]

mkStringMovieAndParticipations :: Movie -> [Participation] -> String
mkStringMovieAndParticipations m ps = let
    movieString = mkStringMovie m
    participationString = map mkStringParticipation ps
  in if null participationString then movieString else unlines $ movieString : participationString where
  mkStringParticipation (Participation p _ pt) = let
      maybeRole = case pt of
        Types.Actor -> ""
        e           -> [i| (#{e})|]
      in [i|\t#{Types._name (p :: Person)}#{maybeRole}|]

parseSeenMovies :: FilePath -> IO ()
parseSeenMovies f = do
  movies <- traverse parse =<< lines <$> readFile f
  traverse_ (withDbPath . FM.addFilteredMovie) movies
  where
    parse :: String -> IO FilteredMovie
    parse line = do
      let (r : id) = unpack $ head $ splitOn "\t" $ pack line
      let reason | r == 'S' = Types.Seen | r == 'I' = Types.Ignored | otherwise = error [i|Unsupported prefix <#{r}>|]
      movie <- getValueOrError $ mkMovieId $ pack id
      return $ FilteredMovie movie reason
    getValueOrError :: MovieId -> IO Movie
    getValueOrError mid = do
      m <- withDbPath $ runMaybeT $ M.getValue mid
      return $ orError [i|Could not find movie with ID <#{mid}>|] m

addFollowedPerson :: String -> IO ()
addFollowedPerson url = do
  let id = parseId $ Url $ pack url
  name <- withKey $ personName id
  _ <- putStrLn [i|Adding <#{name}> and their credits...|]
  let person = Person {_id = id, _name = name}
  _ <- withDbPath $ FP.addFollowedPerson person
  participations <- withKey $ personCredits person
  traverse_ (withDbPath . P.addValueEntry) participations


main :: IO ()
main = do
  args <- Config.parseConfig
  case args of
    (Config.GetUnseen v) -> do
        movies <- getUnseenMovies
        participations <- traverseFproduct getFollowedParticipations movies
        let result = unlines $ if v then map (uncurry mkStringMovieAndParticipations) participations else map mkStringMovie movies
        putStr result
    (Config.UpdateSeen file) -> parseSeenMovies file
    Config.UpdateIndex -> updateMoviesForAllFollowedPersons
    (Config.AddPerson url) -> addFollowedPerson url
