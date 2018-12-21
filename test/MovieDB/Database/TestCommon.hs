module MovieDB.Database.TestCommon (
  withTempDb,
) where

import System.IO.Temp

import Data.Text (Text, pack)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)

import MovieDB.Database.Common (DbPath(..), DbCall)
import Control.Monad.Trans.Reader (runReaderT)


-- Removes the need to explicitly use handle
withTempFile' :: (MonadIO m, MonadMask m) => (Text -> m a) -> m a
withTempFile' f = withTempFile "." "temp_db" $ const . f . pack

-- Since shared in memory doesn't work :\
withTempDb :: DbCall a -> IO a
withTempDb call = withTempFile' $ runReaderT call . DbPath

