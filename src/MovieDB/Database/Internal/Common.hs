{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilies           #-}

module MovieDB.Database.Internal.Common where

import Data.Maybe                    (fromJust)
import Data.Text                     (Text)
import Text.InterpolatedString.Perl6 (qq)

import Control.Arrow                 ((&&&))
import Control.Monad.Trans.Maybe     (MaybeT(..), runMaybeT)

import MovieDB.Database              (DbCall, DbMaybe)

import Database.Persist.Sql          (Unique, entityKey, entityVal, get, getBy)

import Common.Assertions             (assertMsg)
import Common.Operators


class ExtractableId a typeId | a -> typeId where
  extractId :: a -> typeId

class ExtractableId a typeId => ReadOnlyDatabase a typeId rowId | typeId -> rowId a where
  valueAndRowId :: typeId -> DbMaybe (rowId, a)
  -- If someone passes a row ID, it means the value should damn well be there.
  getValueByRowId :: rowId -> DbCall a

class GetUniqueId i r where
  unique :: i -> Unique r
class RowIso e r where
  entityToRow :: e -> r
  rowToEntity :: r -> e

valueAndRowIdImpl entityId = MaybeT $ do
  result <- getBy $ unique entityId
  return $ result <$$> (entityKey &&& rowToEntity . entityVal)
getValueByRowIdImpl id = get id <$$> rowToEntity . fromJust


getValue :: ReadOnlyDatabase a typeId rowId => typeId -> DbMaybe a
getValue id = snd <$> valueAndRowId id

getRowId :: ReadOnlyDatabase a typeId rowId => typeId -> DbMaybe rowId
getRowId id = fst <$> valueAndRowId id

class ReadOnlyDatabase a typeId rowId => ReadWriteDatabase a typeId rowId | typeId a -> rowId where
  forceInsert :: a -> DbCall rowId

-- If there is already a value with the same ID, inserts the value into and returns the rowId (same as forceInsert).
-- If there is a matching ID, checks if the existing value is the same the new value, and throw an exception if
-- it's not. If it is the same, returns the existing rowId.
insertOrVerify :: (ReadWriteDatabase a typeId rowId, Show a, Eq a) => a -> DbCall rowId
insertOrVerify newValue = runMaybeT (valueAndRowId $ extractId newValue) >>= \case
    Nothing -> forceInsert newValue
    Just (rowId, existingValue) -> return $ assertMsg same msg rowId where
      same = existingValue == newValue
      msg = [qq|Existing value <$existingValue> was different from new value <$newValue>|]
