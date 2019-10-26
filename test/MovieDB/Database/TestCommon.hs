module MovieDB.Database.TestCommon (
  withTempDb,
  makePerson,
  makeMovie,
) where

import System.IO.Temp

import Control.Monad.Catch        (MonadMask)
import Control.Monad.IO.Class     (MonadIO)
import Control.Monad.Trans.Reader (runReaderT)

import Data.Text                  (Text, pack)
import Data.Time                  (fromGregorian)

import MovieDB.Database.Common    (DbCall, DbPath(..))
import MovieDB.Types              as T


-- Removes the need to explicitly use handle
withTempFile' :: (MonadIO m, MonadMask m) => (Text -> m a) -> m a
withTempFile' f = withTempFile "." "temp_db" $ const . f . pack

-- Since shared in memory doesn't work :\
withTempDb :: DbCall a -> IO a
withTempDb call = withTempFile' $ runReaderT call . DbPath

makePerson :: Text -> Int -> Person
makePerson name id = T.Person (mkPersonId $ pack $ show id) name

makeMovie :: Text -> Int -> T.Movie
makeMovie name id = T.Movie (mkMovieId $ pack $ show id) name (fromGregorian 2000 1 1)
