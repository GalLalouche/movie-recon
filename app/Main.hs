{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

import           Data.String.Interpolate          (i)
import           Data.Text                        (Text, pack)

import           MovieDB.API                      (ApiKey(..), personCredits, readKey)
import           MovieDB.Database.Common          (DbCall, DbPath(..))
import qualified MovieDB.Database.FollowedPersons as FP
import qualified MovieDB.Database.Movies          as M
import qualified MovieDB.Database.Participations  as P
import qualified MovieDB.Database.SeenMovies      as SM
import           MovieDB.Types                    (Movie(..), MovieId(..), Person(..), PersonId(..))

import           Control.Monad                    ((>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT)

import           Common.MonadPluses               (traverseFilter)
import           Common.Operators

makePerson :: Text -> Text -> Person
makePerson = Person . PersonId

dbPath = DbPath "db.sqlite"

withKey :: ReaderT ApiKey IO a -> IO a
withKey r = liftIO readKey >>= runReaderT r


withDbPath :: Monad m => ReaderT DbPath m a -> m a
withDbPath = flip runReaderT dbPath

updateMoviesForAllFollowedPersons :: IO ()
updateMoviesForAllFollowedPersons = do
  followedPersons <- withDbPath FP.allFollowedPersons
  participations <- withKey $ concat <$> traverse personCredits followedPersons
  withDbPath $ traverse P.addValueEntry participations
  return ()

getUnseenMovies :: IO [Movie]
getUnseenMovies = do
  movies <- withDbPath M.allMovies :: IO [Movie]
  traverseFilter (withDbPath . SM.isNotSeen) movies :: IO [Movie]

mkStringMovies :: [Movie] -> String
mkStringMovies = unlines . map aux where
  aux (Movie (MovieId id) name date) = [i|#{name}, #{date}, (#{id})|]

main :: IO ()
main = do
  movies <- getUnseenMovies
  putStr $ mkStringMovies movies
