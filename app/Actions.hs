{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Actions(
  APIAndDB,
  updateMoviesForAllFollowedPersons,
  addFollowedPerson,
  parseSeenMovies,
  getFormattedUnseenMovies,
  updateScores
) where

import           Data.String.Interpolate          (i)
import           Data.Text                        (pack, splitOn, unpack)

import qualified MovieDB.API                      as API
import           MovieDB.Database.Common          (DbCall, DbPath)
import qualified MovieDB.Database.FilteredMovies  as FM
import qualified MovieDB.Database.FollowedPersons as FP
import qualified MovieDB.Database.Movies          as M
import qualified MovieDB.Database.MovieScores     as MS
import qualified MovieDB.Database.Participations  as P
import           MovieDB.Parsers                  (Url(..), parseId)
import           MovieDB.Types                    (FilteredMovie(..), Movie(..), MovieId(..), Participation(..), Person(..), mkMovieId)
import qualified MovieDB.Types                    as Types
import           OMDB                             (MovieScores)
import qualified OMDB

import           Control.Monad                    ((>=>), unless)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (ExceptT(..), mapExceptT)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT)
import           Data.Foldable                    (traverse_)

import           Common.ExceptTUtils              (meither, toExcept)
import           Common.Maybes                    (orError)
import           Common.MonadPluses               (traverseFilter)
import           Common.Traversables              (traverseFproduct)


type APIAndDB = ReaderT DbPath IO ()
getUnseenMovies :: DbCall [Movie]
getUnseenMovies = M.allMovies >>= traverseFilter FM.isNotFiltered

getFollowedParticipations :: Movie -> DbCall [Participation]
getFollowedParticipations = P.getParticipationsForMovie >=> traverseFilter (FP.isFollowed . Types.person)

liftApi = liftIO

updateMoviesForAllFollowedPersons :: APIAndDB
updateMoviesForAllFollowedPersons = do
  followedPersons <- FP.allFollowedPersons
  participations <- liftApi $ concat <$> traverse API.personCredits followedPersons
  traverse_ P.addValueEntry participations

addFollowedPerson :: String -> APIAndDB
addFollowedPerson url = do
  let id = parseId $ Url $ pack url
  name <- liftApi $ API.personName id
  _ <- liftIO $ putStrLn [i|Adding <#{name}> and their credits...|]
  let person = Person {_id = id, _name = name}
  _ <- FP.addFollowedPerson person
  participations <- liftApi $ API.personCredits person
  traverse_ P.addValueEntry participations

parseSeenMovies :: DbCall ()
parseSeenMovies = do
  ls <- lines <$> liftIO getContents
  movies <- traverse parse ls
  traverse_ FM.addFilteredMovie movies
  where
    parse :: String -> DbCall FilteredMovie
    parse line = do
      let r : id = unpack $ head $ splitOn "\t" $ pack line
      let reason | r == 'S' = Types.Seen | r == 'I' = Types.Ignored | otherwise = error [i|Unsupported prefix <#{r}>|]
      movie <- getValueOrError $ mkMovieId $ pack id
      return $ FilteredMovie movie reason
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
  movies <- getUnseenMovies
  participations <- traverseFproduct getFollowedParticipations movies
  let formattedMovies = map mkStringMovie movies
  let formattedParticipations = map (uncurry mkStringMovieAndParticipations) participations
  liftIO $ putStr $ unlines $ if verbose then formattedParticipations else formattedMovies

updateScores :: APIAndDB
updateScores = traverse_ updateScore =<< M.allMovies where
  updateScore movie = do
    hasScore <- MS.hasMovieScores movie
    unless hasScore (handle $ mapExceptT liftIO $ fetchScores movie)
  fetchScores :: Movie -> ExceptT String IO MovieScores
  fetchScores movie = do
    _ <- liftIO $ putStrLn [i|Fetching scores for <#{movie}>|]
    id <- toExcept [i|No IMDB ID for <#{movie}>!|] (API.imdbId movie)
    toExcept [i|No scores <#{movie}>!|] (OMDB.getMovieScores movie id)
  handle = meither (liftIO . putStrLn) MS.addMovieScores
