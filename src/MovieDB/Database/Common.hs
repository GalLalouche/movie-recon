{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module MovieDB.Database.Common where

import Data.Functor               (($>))
import Data.String.Interpolate    (i)
import Data.Text                  (Text)

import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT)


newtype DbPath = DbPath { path :: Text }
type DbCall = ReaderT DbPath IO
type DbMaybe = MaybeT DbCall

class ExtractableId a typeId | a -> typeId where
  extractId :: a -> typeId

class ExtractableId a typeId => ReadOnlyDatabase a typeId rowId | typeId -> rowId a where
  valueAndRowId :: typeId -> DbMaybe (rowId, a)
  -- If someone passes a row ID, it means the value should damn well be there.
  getValueByRowId :: rowId -> DbCall a

class ReadOnlyDatabase a typeId rowId => ReadWriteDatabase a typeId rowId | typeId a -> rowId where
  -- TODO should be internal
  forceInsert :: a -> DbCall rowId

getValue :: (ReadOnlyDatabase a typeId rowId) => typeId -> DbMaybe a
getValue id = snd <$> valueAndRowId id

getRowId :: ReadOnlyDatabase a typeId rowId => typeId -> DbMaybe rowId
getRowId id = fst <$> valueAndRowId id

-- If there is already a value with the same ID, inserts the value into and returns the rowId (same as forceInsert).
-- If there is a matching ID, checks if the existing value is the same the new value, and throw an exception if
-- it's not. If it is the same, returns the existing rowId.
insertOrVerify :: (ReadWriteDatabase a typeId rowId, Show a, Eq a) => a -> DbCall rowId
insertOrVerify a = do
  oldValue <- runMaybeT $ valueAndRowId $ extractId a
  case oldValue of
    Nothing                     -> forceInsert a
    Just (rowId, existingValue) -> liftIO $ assertSameOrThrow existingValue a $> rowId
  where
    assertSameOrThrow :: (Eq a, Show a) => a -> a -> IO ()
    assertSameOrThrow existingValue newValue =
      if existingValue == newValue then return ()
      else error [i|Existing value <#{existingValue}> was different from new value <#{newValue}>|]
