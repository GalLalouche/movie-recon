{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main.Action(
  JoinedAction,
  updateMoviesForAllFollowedPersons,
  addFollowedPerson,
  DB.parseSeenMovies,
  DB.printUnseenMovies,
  updateScores,
  DB.initDatabases,
) where

import           Prelude                          hiding (lines, putStrLn, unlines)

import           Data.Either.Combinators          (maybeToRight)
import           Data.Foldable                    (fold, traverse_)
import           Data.Text                        (Text)
import           Data.Text.IO                     (putStrLn)
import           Data.Vector                      (Vector)
import           Text.InterpolatedString.Perl6    (qq)

import           Control.Monad                    (unless, (>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT(..), mapExceptT)
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)
import           Data.Functor                     (void)

import           APIs                             (Url(..))
import qualified MovieDB.API                      as API
import           MovieDB.Database                 (toMaybeMaybe, withDbPath)
import qualified MovieDB.Database.ExternalIds     as ExternalIds
import qualified MovieDB.Database.FilteredMovies  as FilteredMovies
import qualified MovieDB.Database.FollowedPersons as FollowedPersons
import qualified MovieDB.Database.Movies          as Movies
import qualified MovieDB.Database.MovieScores     as MovieScores
import           MovieDB.Types                    (ImdbId, Movie(..), Participation(movie))
import qualified MovieDB.Types                    as Types (ExternalHost(IMDB))
import           OMDB                             (MovieScores)
import qualified OMDB

import           Common.ExceptTs                  (meither, toExcept)
import           Common.MonadPluses               (traverseFilter)
import           Common.Operators
import qualified Common.Sets                      as Sets (from)
import qualified Common.Vectors                   as Vectors (from)

import           Main.Action.Internal.API         (JoinedIO)
import qualified Main.Action.Internal.API         as ActionAPI
import qualified Main.Action.Internal.Database    as DB


type JoinedAction = JoinedIO ()
liftApi = liftIO

updateMoviesForAllFollowedPersons :: JoinedAction
updateMoviesForAllFollowedPersons = withDbPath $ do
  followedPersons <- FollowedPersons.allFollowedPersons
  participations <- liftApi $ fold <$> traverse API.personCredits followedPersons
  void $ DB.filterReleasedAndSave participations

addFollowedPerson :: Text -> Bool -> JoinedAction
addFollowedPerson url ignoreActing = getPerson >>= getParticipations <$$> toMovies >>= updateScoresForMovies where
  getPerson = ActionAPI.cache (FollowedPersons.addFollowedPerson ignoreActing) (API.personName $ Url url)
  getParticipations person = withDbPath $ liftApi (API.personCredits person) >>= DB.filterReleasedAndSave
  toMovies = distinct . fmap movie where distinct = Vectors.from . Sets.from

type JoinedError = ExceptT Text JoinedIO
updateScores :: JoinedAction
updateScores = withDbPath Movies.allMovies >>= updateScoresForMovies
updateScoresForMovies :: Vector Movie -> JoinedAction
updateScoresForMovies = withDbPath . traverseFilter FilteredMovies.isNotFiltered >=> traverse_ updateScore where
  updateScore :: Movie -> JoinedAction
  updateScore movie = do
    hasScore <- withDbPath $ MovieScores.hasMovieScores movie
    unless hasScore (handle $ fetchScores movie)
  fetchScores :: Movie -> JoinedError MovieScores
  fetchScores movie = do
    _ <- liftIO $ putStrLn [qq|Fetching scores for <$movie>|]
    id <- getImdbId movie
    mapExceptT lift $ toExcept [qq|No scores <$movie>!|] (OMDB.getMovieScores movie id)
  handle :: JoinedError MovieScores -> JoinedAction
  handle = meither (liftIO . putStrLn) (withDbPath . MovieScores.addMovieScores)
  getImdbId :: Movie -> JoinedError ImdbId
  getImdbId movie = do
    let inserter = ExternalIds.addNullableExternalId movie Types.IMDB
    let dbGetter = toMaybeMaybe <$> ExternalIds.imdbId movie
    let fetcher = runMaybeT $ API.imdbId movie
    lift (ActionAPI.getOrFetch inserter dbGetter fetcher) >>= \case
      ActionAPI.Cached id -> getOrElse [qq|Cached Null IMDB ID for <$movie>!|] id
      ActionAPI.Fetched id -> getOrElse [qq|No IMDB ID could be fetched for <$movie>! (caching...)|] id
    where
      getOrElse = ExceptT .: return .: maybeToRight
