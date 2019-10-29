{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main.Action.Internal.Database(
  filterReleasedAndSave,
  parseSeenMovies,
  printUnseenMovies,
  initDatabases,
) where

import           Prelude                          hiding (lines, putStrLn, unlines)

import           Data.Foldable                    (toList, traverse_)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Ord
import           Data.Text                        (Text, lines, pack, splitOn, unlines, unpack)
import           Data.Text.IO                     (putStrLn)
import           Data.Vector                      (Vector)
import           Text.InterpolatedString.Perl6    (qq)

import           Control.Applicative              (liftA2)
import           Control.Arrow                    ((&&&))
import           Control.Monad                    (mfilter, (>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)

import           MovieDB.Database                 (DbCall)
import qualified MovieDB.Database.ExternalIds     as ExternalIds
import qualified MovieDB.Database.FilteredMovies  as FilteredMovies
import qualified MovieDB.Database.FollowedPersons as FollowedPersons
import qualified MovieDB.Database.Movies          as Movies
import qualified MovieDB.Database.MovieScores     as MovieScores
import qualified MovieDB.Database.Participations  as Participations
import qualified MovieDB.Database.Persons         as Persons
import           MovieDB.Types                    (FilteredMovie(..), Movie(..), Participation(..), mkMovieId)
import qualified MovieDB.Types                    as Types
import           OMDB                             (MovieScore(_score), MovieScores(_scores))

import           Common.Foldables                 (average)
import           Common.IO                        (getCurrentDate)
import           Common.Maybes                    (mapMonoid, orError)
import           Common.MonadPluses               (traverseFilter)
import           Common.Operators
import           Common.Traversables              (traverseFproduct)
import           Common.Vectors                   (sortOn)

import qualified Main.Formatters                  as F


getUnseenMovies :: DbCall (Vector Movie)
getUnseenMovies = Movies.allMovies >>= traverseFilter FilteredMovies.isNotFiltered

filterReleasedAndSave :: Vector Participation -> DbCall (Vector Participation)
filterReleasedAndSave ms = do
  currentDate <- liftIO getCurrentDate
  let isReleased (Participation _ m _) = Types.isReleased currentDate m
  let released = mfilter isReleased ms
  traverse_ Participations.addValueEntry released
  return released

parseSeenMovies :: DbCall ()
parseSeenMovies = do
  ls <- lines . pack <$> liftIO getContents
  movies <- traverse parse ls
  traverse_ FilteredMovies.addFilteredMovie movies
  where
    parse :: Text -> DbCall FilteredMovie
    parse line = do
      let r : id = unpack $ head $ splitOn "\t" line
      let reason | r == 'S' = Types.Seen | r == 'I' = Types.Ignored | otherwise = error [qq|Unsupported prefix <$r>|]
      movie <- getValueOrError $ mkMovieId $ pack id
      return $ FilteredMovie movie reason
    getValueOrError mid = orError [qq|Could not find movie with ID <$mid>|] <$> runMaybeT (Movies.getValue mid)

printUnseenMovies :: Bool -> DbCall ()
printUnseenMovies verbose = do
  movies <- getUnseenMovies
  extraInfo <- traverseFproduct getExtraInfo movies
  let formattedMovies = fmap (`F.mkStringMovie` Nothing) movies
  let formattedParticipations = F.mkFullMovieInfoString . toFullMovieInfo <$> sortOn (Data.Ord.Down . sorter . snd . snd) extraInfo
  liftIO $ putStrLn $ unlines $ toList $ if verbose then formattedParticipations else formattedMovies where
    getFollowedParticipations :: Movie -> DbCall (Vector Participation)
    getFollowedParticipations = Participations.getParticipationsForMovie >=>
        traverseFilter (uncurry FollowedPersons.isFollowed . (Types.participationType &&& Types.person))
    getExtraInfo :: Movie -> DbCall (Vector Participation, Maybe MovieScores)
    getExtraInfo = let
        getScores = runMaybeT . MovieScores.movieScores
      in uncurry (liftA2 (,)) . (getFollowedParticipations &&& getScores)
    toFullMovieInfo (m, (p, ms)) = F.FullMovieInfo m p ms
    sorter :: Maybe MovieScores -> Rational
    sorter = mapMonoid (toList . _scores) >$> _score .> average .> fromMaybe 0

initDatabases :: DbCall()
initDatabases = Movies.init >> Persons.init >> Participations.init >> FilteredMovies.init >> MovieScores.init >> FollowedPersons.init >> ExternalIds.init

