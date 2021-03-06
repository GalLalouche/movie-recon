{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main.Action.Internal.Database(
  filterReleasedAndSave,
  parseSeenMovieLine,
  parseSeenMovies,
  printUnseenMovies,
  initDatabases,
) where

import           Prelude                         hiding (lines, putStrLn, unlines)

import           Data.Foldable                   (toList, traverse_)
import           Data.Maybe                      (fromMaybe)
import qualified Data.Ord
import           Data.Text                       (Text, lines, pack, splitOn, unlines, unpack)
import           Data.Text.IO                    (putStrLn)
import           Data.Vector                     (Vector)
import           Text.InterpolatedString.Perl6   (qq)

import           Control.Applicative             (liftA2)
import           Control.Arrow                   ((&&&), (***))
import           Control.Monad                   (mfilter, (>=>))
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Maybe       (runMaybeT)

import           MovieDB.Database                (DbCall)
import qualified MovieDB.Database.ExternalId     as ExternalId
import qualified MovieDB.Database.FilteredMovie  as FilteredMovie
import qualified MovieDB.Database.FollowedPerson as FollowedPerson
import qualified MovieDB.Database.Movie          as Movie
import qualified MovieDB.Database.MovieScore     as MovieScore
import qualified MovieDB.Database.Participation  as Participation
import qualified MovieDB.Database.Person         as Person
import           MovieDB.Types                   (FilterReason(Ignored, LowScores, Seen), FilteredMovie(FilteredMovie), Movie, MovieId, Participation(Participation, participationType, person), mkMovieId)
import qualified MovieDB.Types                   as Types (isReleased)
import           OMDB                            (MovieScore(_score), MovieScores(_scores))

import           Common.Foldables                (average, notNull)
import           Common.IO                       (getCurrentDate)
import           Common.Maybes                   (mapMonoid, orError)
import           Common.MonadPluses              (traverseFilter)
import           Common.Operators                ((.>), (>$>), (|>))
import qualified Common.Sets                     as Sets
import           Common.Traversables             (traverseFproduct)
import           Common.Vectors                  (sortOn)

import qualified Main.Action.Internal.Format     as F


filterReleasedAndSave :: Vector Participation -> DbCall (Vector Participation)
filterReleasedAndSave ms = do
  currentDate <- liftIO getCurrentDate
  let isReleased (Participation _ m _) = Types.isReleased currentDate m
  let released = mfilter isReleased ms
  traverse_ Participation.addValueEntry released
  return released

parseSeenMovieLine :: Text -> (MovieId, FilterReason)
parseSeenMovieLine line = let
    r : id = unpack $ head $ splitOn "\t" line
    reason | r == 'S' = Seen | r == 'I' = Ignored | r == 'L' = LowScores | otherwise = error [qq|Unsupported prefix <$r>|]
  in (mkMovieId $ pack id, reason)

liftUncurry = uncurry . liftA2

parseSeenMovies :: DbCall ()
parseSeenMovies = do
  ls <- lines . pack <$> liftIO getContents
  movies <- traverse parse ls
  traverse_ FilteredMovie.addFilteredMovie movies
  where
    parse :: Text -> DbCall FilteredMovie
    parse = liftUncurry FilteredMovie . (getMovie *** return) . parseSeenMovieLine
    getMovie mid = orError [qq|Could not find movie with ID <$mid>|] <$> runMaybeT (Movie.getValue mid)

printUnseenMovies :: Bool -> DbCall ()
printUnseenMovies verbose = do
  movies <- getMovies
  let formattedMovies = flip F.mkStringMovie Nothing . F.movie <$> movies
  let formattedVerbose = F.mkFullMovieInfoString <$> movies
  liftIO $ putStrLn $ unlines $ toList $ if verbose then formattedVerbose else formattedMovies where
    -- Returns not-filtered movies, with non-empty participation of followed persons, sorted by score.
    getMovies :: DbCall (Vector F.FullMovieInfo)
    getMovies = do
      notFiltered <- Movie.getAll >>= traverseFilter FilteredMovie.isNotFiltered
      fullInfo <- fmap toFullMovieInfo <$> traverseFproduct getExtraInfo notFiltered
      let moviesWithFollowedParticipations = mfilter (notNull . F.participations) fullInfo
      let sortedByScore = sortOn (Data.Ord.Down . sorter . F.scores) moviesWithFollowedParticipations
      return sortedByScore
    getFollowedParticipations :: Movie -> DbCall (Vector Participation)
    getFollowedParticipations = Participation.getParticipationsForMovie >=>
        traverseFilter (uncurry FollowedPerson.isFollowed . (participationType &&& person))
    getExtraInfo :: Movie -> DbCall (Vector Participation, Maybe MovieScores)
    getExtraInfo = liftUncurry (,) . (getFollowedParticipations &&& runMaybeT . MovieScore.movieScores)
    toFullMovieInfo (m, (p, ms)) = F.FullMovieInfo m p ms
    sorter :: Maybe MovieScores -> Rational
    sorter = (mapMonoid _scores Sets.>$> _score) .> average .> fromMaybe 0

initDatabases :: DbCall ()
initDatabases = sequence_ [Movie.init, Person.init, Participation.init, FilteredMovie.init, MovieScore.init, FollowedPerson.init, ExternalId.init]
