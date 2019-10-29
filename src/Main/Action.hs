{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main.Action(
  APIAndDB,
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
import           Control.Monad.Trans.Except       (ExceptT(..), mapExceptT, throwE)
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT)
import           Data.Functor                     (void)

import           APIs                             (Url(..))
import qualified MovieDB.API                      as API
import           MovieDB.Database                 (DbPath, withDbPath)
import qualified MovieDB.Database.ExternalIds     as ExternalIds
import qualified MovieDB.Database.FilteredMovies  as FilteredMovies
import qualified MovieDB.Database.FollowedPersons as FollowedPersons
import qualified MovieDB.Database.Movies          as Movies
import qualified MovieDB.Database.MovieScores     as MovieScores
import           MovieDB.Types                    (Movie(..), Participation(..))
import qualified MovieDB.Types                    as Types
import           OMDB                             (MovieScores)
import qualified OMDB

import           Common.ExceptTs                  (meither, toExcept)
import           Common.MonadPluses               (traverseFilter)
import qualified Common.Sets                      as Sets (from)
import qualified Common.Vectors                   as Vectors (from)

import qualified Main.Action.Internal.Database    as DB


type IAPIAndDB = ReaderT DbPath IO
type APIAndDB = IAPIAndDB ()
liftApi = liftIO

updateMoviesForAllFollowedPersons :: APIAndDB
updateMoviesForAllFollowedPersons = withDbPath $ do
  followedPersons <- FollowedPersons.allFollowedPersons
  participations <- liftApi $ fold <$> traverse API.personCredits followedPersons
  void $ DB.filterReleasedAndSave participations

addFollowedPerson :: Text -> Bool -> APIAndDB
addFollowedPerson url ignoreActing = getPerson >>= getMovies >>= updateScoresForMovies where
  getPerson = withDbPath $ do
    person <- liftApi $ API.personName $ Url url
    _ <- liftIO $ putStrLn [qq|Adding <$person> and their credits and scores...|]
    _ <- FollowedPersons.addFollowedPerson ignoreActing person
    return person
  getMovies person = withDbPath $ do
    participations <- liftApi $ API.personCredits person
    releasedParticipations <- DB.filterReleasedAndSave participations
    return $ distinct $ fmap (Types.movie :: Participation -> Movie) releasedParticipations where
      distinct = Vectors.from . Sets.from

updateScores :: APIAndDB
updateScores = withDbPath Movies.allMovies >>= updateScoresForMovies
updateScoresForMovies :: Vector Movie -> APIAndDB
updateScoresForMovies = withDbPath . traverseFilter FilteredMovies.isNotFiltered >=> traverse_ updateScore where
  updateScore :: Movie -> APIAndDB
  updateScore movie = do
    hasScore <- withDbPath $ MovieScores.hasMovieScores movie
    unless hasScore (handle $ fetchScores movie)
  fetchScores :: Movie -> ExceptT Text IAPIAndDB MovieScores
  fetchScores movie = do
    _ <- liftIO $ putStrLn [qq|Fetching scores for <$movie>|]
    id <- getImdbId movie
    mapExceptT lift $ toExcept [qq|No scores <$movie>!|] (OMDB.getMovieScores movie id)
  handle :: ExceptT Text IAPIAndDB MovieScores -> APIAndDB
  handle = meither (liftIO . putStrLn) (withDbPath . MovieScores.addMovieScores)
  getImdbId :: Movie -> ExceptT Text IAPIAndDB Types.ImdbId
  getImdbId movie = do
    let liftDbPath = lift . withDbPath
    liftDbPath (ExternalIds.imdbId movie) >>= \case
      ExternalIds.NotNull id -> return id
      ExternalIds.Null -> throwE [qq|Cached Null IMDB ID for <$movie>!|]
      ExternalIds.NoRow -> do
        -- Use monad transformers they said. You'd need less conversions they said.
        fetchedId <- lift $ lift $ runMaybeT $ API.imdbId movie
        _ <- liftDbPath $ ExternalIds.addNullableExternalId movie Types.IMDB fetchedId
        ExceptT $ return $ maybeToRight [qq|No IMDB ID could be fetched for <$movie>! (caching...)|] fetchedId
