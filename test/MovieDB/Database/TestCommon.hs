{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.TestCommon (
  withTempDb,
  makePerson,
  makeMovie,
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

makePerson :: Text -> Person
makePerson name = T.Person (T.PersonId $ name <> "Id") name

makeMovie :: Text -> T.Movie
makeMovie name = T.Movie (T.MovieId $ name <> "Id") name
