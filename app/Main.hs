{-# LANGUAGE OverloadedStrings #-}

import           Data.Text                        (Text)
import           MovieDB.API                      (ApiKey(..), personCredits, readKey)
import           MovieDB.Database.Common          (DbPath(..))
import qualified MovieDB.Database.FollowedPersons as FP
import qualified MovieDB.Database.Participations  as P
import qualified MovieDB.Types                    as Types

import           Control.Monad                    ((>=>))
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT)

import Common.Operators

makePerson :: Text -> Text -> Types.Person
makePerson = Types.Person . Types.PersonId

dbPath = DbPath "db.sqlite"

withKey :: ReaderT ApiKey IO a -> IO a
withKey r = liftIO readKey >>= runReaderT r
  
  
withDbPath :: Monad m => ReaderT DbPath m a -> m a
withDbPath = flip runReaderT dbPath

updateMoviesForAllFollowedPersons = do
  followedPersons <- withDbPath FP.allFollowedPersons
  participations <- withKey $ concat <$> traverse personCredits followedPersons
  withDbPath $ traverse P.addValueEntry participations

main :: IO ()
main = do
  updateMoviesForAllFollowedPersons
  return ()
