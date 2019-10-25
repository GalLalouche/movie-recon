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
import           MovieDB.Types                    (FilteredMovie(..), Movie(..), MovieId(..), Participation(..), Person(..), mkMovieId)
import qualified MovieDB.Types                    as Types
import           OMDB                             (MovieScore(..), MovieScores, Source(IMDB, Metacritic, RottenTomatoes))
import qualified OMDB

import           Control.Applicative              (liftA2)
import           Control.Arrow                    ((&&&))
import           Control.Monad                    (unless, (>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (ExceptT(..), mapExceptT)
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT)
import           Data.Foldable                    (toList, traverse_)

import           Common.ExceptTUtils              (meither, toExcept)
import           Common.Foldables                 (intercalate)
import           Common.Maybes                    (mapMonoid, orError)
import           Common.MonadPluses               (traverseFilter)
import           Common.Operators
import           Common.Traversables              (traverseFproduct)


type APIAndDB = ReaderT DbPath IO ()
getUnseenMovies :: DbCall [Movie]
getUnseenMovies = M.allMovies >>= traverseFilter FM.isNotFiltered

liftApi = liftIO

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

mkStringMovie :: Movie -> Maybe MovieScores -> String
mkStringMovie (Movie (MovieId id) name date) ms = let
    scoreString = maybe "No score" (toString . toList . OMDB._scores) ms
    toString ms = [i|(#{intercalate ", " $ fmap aux ms})|]
    aux (MovieScore source score) = let
      shortSource = case source of
        IMDB           -> "IMDB"
        RottenTomatoes -> "RT"
        Metacritic     -> "MC"
      in [i|#{score} #{shortSource}|]
  in [i|#{id}\t#{name}\t#{date} #{scoreString}|]

printUnseenMovies :: Bool -> DbCall ()
printUnseenMovies verbose = do
  movies <- getUnseenMovies
  extraInfo <- traverseFproduct getExtraInfo movies
  let formattedMovies = map (`mkStringMovie` Nothing) movies
  let formattedParticipations = map toString $ sortOn (Data.Ord.Down . sorter . snd . snd) extraInfo
  liftIO $ putStr $ unlines $ if verbose then formattedParticipations else formattedMovies where
    getFollowedParticipations :: Movie -> DbCall [Participation]
    getFollowedParticipations = P.getParticipationsForMovie >=> traverseFilter (FP.isFollowed . Types.person)
    getScores :: Movie -> DbCall (Maybe MovieScores)
    getScores = runMaybeT . MS.movieScores
    getExtraInfo :: Movie -> DbCall ([Participation], Maybe MovieScores)
    getExtraInfo = uncurry (liftA2 (,)) . (getFollowedParticipations &&& getScores)
    toString :: (Movie, ([Participation], Maybe MovieScores)) -> String
    toString (m, (ps, ms)) = let
        movieString = mkStringMovie m ms
        participationStrings = map mkStringParticipation ps
      in unlines $ movieString : participationStrings where
      mkStringParticipation (Participation p _ pt) = let
          maybeRole = case pt of
            Types.Actor -> ""
            e           -> [i| (#{e})|]
          name = Types._name (p :: Person)
        in [i|\t#{name}#{maybeRole}|]
    sorter :: Maybe MovieScores -> Double
    sorter = mapMonoid (toList . OMDB._scores) >$> _score .> average
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
