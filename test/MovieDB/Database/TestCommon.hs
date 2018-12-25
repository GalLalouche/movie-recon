{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.TestCommon (
  withTempDb,
  unsafeMakePerson,
  unsafeMakeMovie,
) where

import System.IO.Temp

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text, pack)

import Control.Monad.Trans.Reader (runReaderT)
import Data.Monoid ((<>))

import MovieDB.Database.Common (DbCall, DbPath(..))
import MovieDB.Types as T

-- Removes the need to explicitly use handle
withTempFile' :: (MonadIO m, MonadMask m) => (Text -> m a) -> m a
withTempFile' f = withTempFile "." "temp_db" $ const . f . pack

-- Since shared in memory doesn't work :\
withTempDb :: DbCall a -> IO a
withTempDb call = withTempFile' $ runReaderT call . DbPath

unsafeMakePerson :: T.Person p => Text -> p
unsafeMakePerson name = makePerson (T.PersonId $ name <> "Id") name

unsafeMakeMovie :: Text -> T.Movie
unsafeMakeMovie name = T.Movie (T.MovieId $ name <> "Id") name
