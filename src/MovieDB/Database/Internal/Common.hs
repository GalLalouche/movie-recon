{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}

module MovieDB.Database.Internal.Common where

import Text.InterpolatedString.Perl6 (qq)

import Control.Monad.IO.Class        (liftIO)
import Control.Monad.Trans.Maybe     (runMaybeT)
import Data.Functor                  (($>))

import MovieDB.Database              (DbCall, DbMaybe)


class ExtractableId a typeId | a -> typeId where
  extractId :: a -> typeId

class ExtractableId a typeId => ReadOnlyDatabase a typeId rowId | typeId -> rowId a where
  valueAndRowId :: typeId -> DbMaybe (rowId, a)
  -- If someone passes a row ID, it means the value should damn well be there.
  getValueByRowId :: rowId -> DbCall a

class ReadOnlyDatabase a typeId rowId => ReadWriteDatabase a typeId rowId | typeId a -> rowId where
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
      else error [qq|Existing value <$existingValue> was different from new value <$newValue>|]
