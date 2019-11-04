{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module MovieDB.Database.Internal.Common where

import           Data.Maybe                    (fromJust)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as Vector (fromList)

import           Text.InterpolatedString.Perl6 (qq)

import           Control.Arrow                 ((&&&))
import           Control.Monad.Trans.Maybe     (MaybeT(..), runMaybeT)

import           MovieDB.Database              (DbCall, DbMaybe)

import           Database.Persist.Sql          (Key, PersistEntity, PersistEntityBackend, SqlBackend, Unique, entityKey, entityVal, get, getBy, insert, selectList)

import           Common.Assertions             (assertMsg)
import           Common.Operators              ((<$$>), (<$<))


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
  -- If someone passes a row ID, then the value should damn well be there.
getValueByRowId :: RowIso e eId r => Key r -> DbCall e
getValueByRowId id = get id <$$> rowToEntity . fromJust

getAll :: RowIso e eId r => DbCall (Vector e)
getAll = Vector.fromList . fmap (rowToEntity . entityVal) <$> selectList [] []


forceInsert :: RowIso e eId r => e -> DbCall (Key r)
forceInsert = insert . entityToRow
-- If there is already a value with the same ID, inserts the value into and returns the rowId (same as forceInsert).
-- If there is a matching ID, checks if the existing value is the same the new value, and throw an exception if
-- it's not. If it is the same, returns the existing rowId.
insertOrVerify :: (RowIso a typeId r, Show a, Eq a) => a -> DbCall (Key r)
insertOrVerify newValue = runMaybeT (valueAndRowId $ extractId newValue) >>= \case
    Nothing -> forceInsert newValue
    Just (rowId, existingValue) -> return $ assertMsg same msg rowId where
      same = existingValue == newValue
      msg = [qq|Existing value <$existingValue> was different from new value <$newValue>|]

class ToKey s r | s -> r where
  getKeyFor :: s -> DbCall (Key r)
instance (Eq e, Show e, RowIso e eId r) => ToKey e r where
  getKeyFor = insertOrVerify
instance {-# OVERLAPPING #-} (RowIso e eId r) => ToKey (Key r) r where
  getKeyFor = return
