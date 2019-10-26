{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Actions(
  APIAndDB,
  updateMoviesForAllFollowedPersons,
  addFollowedPerson,
  parseSeenMovies,
  printUnseenMovies,
  updateScores
) where

import           Data.Foldable                    (toList, traverse_)
import           Data.List                        (sortOn)
import qualified Data.Ord
import           Data.String.Interpolate          (i)
import           Data.Text                        (pack, splitOn, unpack)

import           APIs                             (Url(..))
import qualified MovieDB.API                      as API
import           MovieDB.Database.Common          (DbCall, DbPath)
import qualified MovieDB.Database.FilteredMovies  as FM
import qualified MovieDB.Database.FollowedPersons as FP
import qualified MovieDB.Database.Movies          as M
import qualified MovieDB.Database.MovieScores     as MS
import qualified MovieDB.Database.Participations  as P
import           MovieDB.Types                    (FilteredMovie(..), Movie(..), Participation(..), Person(..), mkMovieId)
import qualified MovieDB.Types                    as Types
import           OMDB                             (MovieScore(_score), MovieScores)
import qualified OMDB

import           Control.Applicative              (liftA2)
import           Control.Arrow                    ((&&&))
import           Control.Monad                    (unless, (>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (ExceptT(..), mapExceptT)
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT)

import           Common.ExceptTUtils              (meither, toExcept)
import           Common.Maybes                    (mapMonoid, orError)
import           Common.MonadPluses               (traverseFilter)
import           Common.Operators
import           Common.Traversables              (traverseFproduct)

import qualified Formatters                       as F


type APIAndDB = ReaderT DbPath IO ()
liftApi = liftIO

getUnseenMovies :: DbCall [Movie]
getUnseenMovies = M.allMovies >>= traverseFilter FM.isNotFiltered

updateMoviesForAllFollowedPersons :: APIAndDB
updateMoviesForAllFollowedPersons = do
  followedPersons <- FP.allFollowedPersons
  participations <- liftApi $ concat <$> traverse API.personCredits followedPersons
  traverse_ P.addValueEntry participations

addFollowedPerson :: String -> APIAndDB
addFollowedPerson url = do
  person <- liftApi $ API.personName $ Url url
  _ <- liftIO $ putStrLn [i|Adding <#{person}> and their credits...|]
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

printUnseenMovies :: Bool -> DbCall ()
printUnseenMovies verbose = do
  movies <- getUnseenMovies
  extraInfo <- traverseFproduct getExtraInfo movies
  let formattedMovies = map (`F.mkStringMovie` Nothing) movies
  let formattedParticipations = map (F.mkFullMovieInfoString . toFullMovieInfo) $ sortOn (Data.Ord.Down . sorter . snd . snd) extraInfo
  liftIO $ putStr $ unlines $ if verbose then formattedParticipations else formattedMovies where
    getFollowedParticipations :: Movie -> DbCall [Participation]
    getFollowedParticipations = P.getParticipationsForMovie >=> traverseFilter (FP.isFollowed . Types.person)
    getExtraInfo :: Movie -> DbCall ([Participation], Maybe MovieScores)
    getExtraInfo = let
        getScores = runMaybeT . MS.movieScores
      in uncurry (liftA2 (,)) . (getFollowedParticipations &&& getScores)
    toFullMovieInfo (m, (p, ms)) = F.FullMovieInfo m p ms
    sorter :: Maybe MovieScores -> Double
    sorter = mapMonoid (toList . OMDB._scores) >$> _score .> average where
      -- TODO move to Common
      average v = if null v then 0 else fromIntegral (sum v) / fromIntegral (length v)

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
