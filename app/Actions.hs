{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Actions(
  APIAndDB,
  updateMoviesForAllFollowedPersons,
  addFollowedPerson,
  parseSeenMovies,
  printUnseenMovies,
  updateScores,
  initDatabases,
) where

import           Prelude                          hiding (lines, putStrLn, unlines)

import           Data.Foldable                    (fold, toList, traverse_)
import qualified Data.Ord
import           Data.Text                        (Text, lines, pack, splitOn, unlines, unpack)
import           Data.Text.IO                     (putStrLn)
import           Data.Vector                      (Vector)
import           Text.InterpolatedString.Perl6    (qq)

import           Control.Applicative              (liftA2)
import           Control.Arrow                    ((&&&))
import           Control.Monad                    (unless, (>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (ExceptT(..), mapExceptT)
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT)

import           APIs                             (Url(..))
import qualified MovieDB.API                      as API
import           MovieDB.Database                 (DbCall, DbPath, withDbPath)
import qualified MovieDB.Database.ExternalIds     as ExternalIds
import qualified MovieDB.Database.FilteredMovies  as FilteredMovies
import qualified MovieDB.Database.FollowedPersons as FollowedPersons
import qualified MovieDB.Database.Movies          as Movies
import qualified MovieDB.Database.MovieScores     as MovieScores
import qualified MovieDB.Database.Participations  as Participations
import qualified MovieDB.Database.Persons         as Persons
import           MovieDB.Types                    (FilteredMovie(..), Movie(..), Participation(..), Person(..), mkMovieId)
import qualified MovieDB.Types                    as Types
import           OMDB                             (MovieScore(_score), MovieScores(_scores))
import qualified OMDB

import           Common.ExceptTs                  (meither, toExcept)
import           Common.Maybes                    (mapMonoid, orError)
import           Common.MonadPluses               (traverseFilter)
import           Common.Operators
import           Common.Traversables              (traverseFproduct)
import           Common.Vectors                   (sortOn)

import qualified Formatters                       as F


type APIAndDB = ReaderT DbPath IO ()
liftApi = liftIO

getUnseenMovies :: DbCall (Vector Movie)
getUnseenMovies = Movies.allMovies >>= traverseFilter FilteredMovies.isNotFiltered

updateMoviesForAllFollowedPersons :: APIAndDB
updateMoviesForAllFollowedPersons = withDbPath $ do
  followedPersons <- FollowedPersons.allFollowedPersons
  participations <- liftApi $ fold <$> traverse API.personCredits followedPersons
  traverse_ Participations.addValueEntry participations

addFollowedPerson :: Text -> APIAndDB
addFollowedPerson url = withDbPath $ do
  person <- liftApi $ API.personName $ Url url
  _ <- liftIO $ putStrLn [qq|Adding <$person> and their credits...|]
  _ <- FollowedPersons.addFollowedPerson person
  participations <- liftApi $ API.personCredits person
  traverse_ Participations.addValueEntry participations

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
    getFollowedParticipations = Participations.getParticipationsForMovie >=> traverseFilter (FollowedPersons.isFollowed . Types.person)
    getExtraInfo :: Movie -> DbCall (Vector Participation, Maybe MovieScores)
    getExtraInfo = let
        getScores = runMaybeT . MovieScores.movieScores
      in uncurry (liftA2 (,)) . (getFollowedParticipations &&& getScores)
    toFullMovieInfo (m, (p, ms)) = F.FullMovieInfo m p ms
    sorter :: Maybe MovieScores -> Double
    sorter = mapMonoid (toList . _scores) >$> _score .> average where
      -- TODO move to Common
      average v = if null v then 0 else fromIntegral (sum v) / fromIntegral (length v)

updateScores :: APIAndDB
updateScores = withDbPath $ traverse_ updateScore =<< Movies.allMovies where
  updateScore movie = do
    hasScore <- MovieScores.hasMovieScores movie
    unless hasScore (handle $ mapExceptT liftIO $ fetchScores movie)
  fetchScores :: Movie -> ExceptT Text IO MovieScores
  fetchScores movie = do
    _ <- liftIO $ putStrLn [qq|Fetching scores for <$movie>|]
    id <- toExcept [qq|No IMDB ID for <$movie>!|] (API.imdbId movie)
    toExcept [qq|No scores <$movie>!|] (OMDB.getMovieScores movie id)
  handle = meither (liftIO . putStrLn) MovieScores.addMovieScores

initDatabases :: DbCall()
initDatabases = Movies.init >> Persons.init >> Participations.init >> FilteredMovies.init >> MovieScores.init >> FollowedPersons.init >> ExternalIds.init
