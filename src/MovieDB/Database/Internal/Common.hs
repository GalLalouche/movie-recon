{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances           #-}

module MovieDB.Database.Internal.Common where

import Data.Maybe                    (fromJust)
import Text.InterpolatedString.Perl6 (qq)

import Control.Arrow                 ((&&&))
import Control.Monad.Trans.Maybe     (MaybeT(..), runMaybeT)

import MovieDB.Database              (DbCall, DbMaybe)

import Database.Persist.Sql          (Key, PersistEntity, PersistEntityBackend, SqlBackend, Unique, entityKey, entityVal, get, getBy)

import Common.Assertions             (assertMsg)
import Common.Operators


class (PersistEntity row, PersistEntityBackend row ~ SqlBackend) =>
    RowIso entity entityId row
    | entity -> entityId row
    , entityId -> entity row
    , row -> entity entityId where
  extractId :: entity -> entityId
  unique :: entityId -> Unique row
  entityToRow :: entity -> row
  rowToEntity :: row -> entity

valueAndRowId :: RowIso e eId r => eId -> DbMaybe (Key r, e)
valueAndRowId entityId = MaybeT $ do
  result <- getBy $ unique entityId
  return $ result <$$> (entityKey &&& rowToEntity . entityVal)

getRowId :: RowIso e eId r => eId -> DbMaybe (Key r)
getRowId = fst <$< valueAndRowId
getValue :: RowIso e eId r => eId -> DbMaybe e
getValue = snd <$< valueAndRowId

getValueByRowId id = get id <$$> rowToEntity . fromJust

class RowIso e eId r => ReadWriteDatabase e eId r where
  forceInsert :: e -> DbCall (Key r)

class ToKey s r where
  getKeyFor :: s -> DbCall (Key r)
instance (Eq e, Show e, ReadWriteDatabase e eId r) => ToKey e r where
  getKeyFor = insertOrVerify
instance (ReadWriteDatabase e eId r) => ToKey (Key r) r where
  getKeyFor = return


-- If there is already a value with the same ID, inserts the value into and returns the rowId (same as forceInsert).
-- If there is a matching ID, checks if the existing value is the same the new value, and throw an exception if
-- it's not. If it is the same, returns the existing rowId.
insertOrVerify :: (ReadWriteDatabase a typeId r, Show a, Eq a) => a -> DbCall (Key r)
insertOrVerify newValue = runMaybeT (valueAndRowId $ extractId newValue) >>= \case
    Nothing -> forceInsert newValue
    Just (rowId, existingValue) -> return $ assertMsg same msg rowId where
      same = existingValue == newValue
      msg = [qq|Existing value <$existingValue> was different from new value <$newValue>|]
