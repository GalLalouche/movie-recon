{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

import           Data.String.Interpolate          (i)
import           Data.Text                        (pack, splitOn)

import           MovieDB.API                      (ApiKey(..), personCredits, personName, readKey)
import           MovieDB.Database.Common          (DbPath(..))
import qualified MovieDB.Database.FollowedPersons as FP
import qualified MovieDB.Database.Movies          as M
import qualified MovieDB.Database.Participations  as P
import qualified MovieDB.Database.SeenMovies      as SM
import           MovieDB.Parsers                  (Url(..), parseId)
import           MovieDB.Types                    (Movie(..), MovieId(..), Participation(..), Person(..))
import qualified MovieDB.Types                    as Types

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT)
import           Data.Foldable                    (traverse_)
import           Data.Functor                     (void)

import           System.Console.CmdArgs           (argPos, def, help, typ, typFile, (&=))
import qualified System.Console.CmdArgs           as Cmd
import           System.Console.CmdArgs.Implicit

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
  traverseFilter (withDbPath . SM.isNotSeen) movies :: IO [Movie]

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

data Config = UpdateSeen {seenFile :: FilePath}
            | GetUnseen {verbose :: Bool}
            | UpdateIndex
            | AddPerson {url :: String}
            deriving (Show, Cmd.Data, Cmd.Typeable, Eq)
updateSeenConfig = UpdateSeen {seenFile = def &= typFile &= argPos 0} &=
    help ("Reads a file of seen movie IDs to update seen movies.\n" ++
          "Every line should start with an ID, and optionally more text separated by <TAB>(i.e., the output format of updateseen). Example line:\n" ++
          "\"299536<TAB>Avengers: Infinity War<TAB>2018-04-27\""
    )
getUnseenConfig = GetUnseen { verbose = def &= help "If true, also prints the followed cast and crew for the film"}
    &= help "Return all unseen movies, their release date, and their IDs."
updateIndex = UpdateIndex &= help "Updates the index of movies for all followed persons."
addPerson = AddPerson {url = def &= argPos 0 &= typ "MovieDB URL, e.g., https://www.themoviedb.org/person/17697-john-krasinski"}
    &= help "Adds a followed person"

parseSeenMovies :: FilePath -> IO ()
parseSeenMovies f = do
  ids <- fmap parseId . lines <$> readFile f
  movies <- traverse getValueOrError ids
  traverse_ (withDbPath . SM.addSeenMovie) movies
  where
    parseId :: String -> MovieId
    parseId = MovieId . head . splitOn "\t" . pack
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
  args <- Cmd.cmdArgs $ modes [updateSeenConfig, getUnseenConfig, updateIndex, addPerson]
  case args of
    (GetUnseen v) -> do
        movies <- getUnseenMovies
        participations <- traverseFproduct getFollowedParticipations movies
        let result = unlines $ if v then map (uncurry mkStringMovieAndParticipations) participations else map mkStringMovie movies
        putStr result
        return ()
    (UpdateSeen file) -> parseSeenMovies file
    UpdateIndex -> updateMoviesForAllFollowedPersons
    (AddPerson url) -> addFollowedPerson url

