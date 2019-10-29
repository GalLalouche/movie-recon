{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}


module Main.Action.Internal.API where

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor               (void)

import APIs                       (ApiCall)
import MovieDB.Database           (DbCall, DbPath, withDbPath)
import qualified MovieDB.Database.ExternalIds as Types

-- Both DbCall and ApiCall
type JoinedIO = ReaderT DbPath IO
liftApi = liftIO

type Inserter a b = a -> DbCall b

cache :: Inserter a b -> ApiCall a -> JoinedIO a
cache insert api = do
  remoteValue <- liftApi api
  void $ withDbPath $ insert remoteValue
  return remoteValue

data CacheResult a = Cached a | Fetched a
getOrFetch :: Inserter a b -> DbCall (Maybe a) -> ApiCall a -> JoinedIO (CacheResult a)
getOrFetch inserter db api = do
  existingValue <- withDbPath db
  case existingValue of
    Just x  -> return $ Cached x
    Nothing -> Fetched <$> cache inserter api
