module MovieDB.Database where

import Data.Text                    (Text)

import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Logger         (NoLoggingT)
import Control.Monad.Trans.Maybe    (MaybeT)
import Control.Monad.Trans.Reader   (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Resource (ResourceT)

import Database.Persist.Sql         (SqlBackend)
import Database.Persist.Sqlite      (runSqlite)


type DbCall = ReaderT SqlBackend (NoLoggingT (ResourceT IO))
type DbMaybe = MaybeT DbCall
newtype DbPath = DbPath { path :: Text }
type RunDB = ReaderT DbPath IO

withDbPath :: DbCall a -> ReaderT DbPath IO a
withDbPath action = path <$> ask >>= liftIO . flip runSqlite action

runDbCall :: DbCall a -> DbPath -> IO a
runDbCall = runReaderT . withDbPath
