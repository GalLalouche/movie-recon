{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, QuasiQuotes, FunctionalDependencies, RankNTypes, LambdaCase, ScopedTypeVariables #-}

module MovieDB.Database.Common where

import Common.Operators
import Data.Functor (($>))
import Data.Text (Text)
import Data.String.Interpolate (i)

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Database.Persist (entityKey, entityVal)

newtype DbPath = DbPath { path :: Text }
type DbCall a = ReaderT DbPath IO a
type DbMaybe a = MaybeT (ReaderT DbPath IO) a


class ExtractableId a typeId | a -> typeId where
  extractId :: a -> typeId

class ExtractableId a typeId => ReadOnlyDatabase a typeId rowId | typeId -> rowId a where
  keyVal :: typeId -> DbMaybe (rowId, a)
  getValueByRowId :: rowId -> DbMaybe a

class ReadOnlyDatabase a typeId rowId => ReadWriteDatabase a typeId rowId | typeId a -> rowId where
  -- TODO should be internal
  forceInsert :: a -> DbCall rowId

getValue :: (ReadOnlyDatabase a typeId rowId) => typeId -> DbMaybe a
getValue id = snd <$> keyVal id

getKey :: ReadOnlyDatabase a typeId rowId => typeId -> DbMaybe rowId
getKey id = fst <$> keyVal id

-- If there is already a value with the same ID, inserts the value into and returns the rowId (same as forceInsert).
-- If there is a matching ID, checks if the existing value is the same the new value, and throw an exception if
-- it's not. If it is the same, returns the existing rowId.
insertOrVerify :: (ReadWriteDatabase a typeId rowId, Show a, Eq a) => a -> DbCall rowId
insertOrVerify a = do
  oldValue <- runMaybeT $ keyVal $ extractId a
  case oldValue of
    Nothing -> forceInsert a
    Just (rowId, existingValue) -> liftIO $ assertSameOrThrow existingValue a $> rowId
  where
    assertSameOrThrow :: (Eq a, Show a) => a -> a -> IO ()
    assertSameOrThrow existingValue newValue =
      if existingValue == newValue then return ()
      else error [i|Existing value <#{existingValue}> was different from new value <#{newValue}>|]


