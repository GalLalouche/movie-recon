{-# LANGUAGE DeriveFunctor #-}

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

data Nullable a = NoRow | Null | NotNull a deriving (Show, Eq, Ord, Functor)
toMaybeMaybe :: Nullable a -> Maybe (Maybe a)
toMaybeMaybe NoRow       = Nothing
toMaybeMaybe Null        = Just Nothing
toMaybeMaybe (NotNull a) = Just $ Just a
fromMaybeMaybe :: Maybe (Maybe a) -> Nullable a
fromMaybeMaybe Nothing         = NoRow
fromMaybeMaybe (Just Nothing)  = Null
fromMaybeMaybe (Just (Just a)) = NotNull a
