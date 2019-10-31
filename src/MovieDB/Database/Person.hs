{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.Person(
  init,
  clear,
  PersonRowId,
  PersonRowable,
  toPersonRowId,
) where

import Prelude                          hiding (id, init)

import Data.Text                        (Text)

import Control.Lens                     (Iso', classUnderscoreNoPrefixFields, from, iso, makeLensesWith, view, (^.))
import Data.Functor                     (void)

import MovieDB.Database                 (DbCall)
import MovieDB.Database.Internal.Common (ExtractableId(..), GetUniqueId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), getValueByRowIdImpl, insertOrVerify, valueAndRowIdImpl, RowIso(..))
import MovieDB.Types                    (Person(..), PersonId, mkPersonId)

import Database.Persist.Sql             (Filter, deleteWhere, insert, runMigrationSilent)
import Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
PersonRow sql=person
  personId        Text
  UniquePersonId  personId
  name            Text
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

clear :: DbCall ()
clear = deleteWhere ([] :: [Filter PersonRow])

makeLensesWith classUnderscoreNoPrefixFields ''PersonId
makeLensesWith classUnderscoreNoPrefixFields ''Person

instance RowIso Person PersonRow where
  entityToRow person = PersonRow (person ^. id ^. id) (person ^. name)
  rowToEntity (PersonRow id name) = Person (mkPersonId id) name

instance ExtractableId Person PersonId where extractId = view id
instance GetUniqueId PersonId PersonRow where unique = UniquePersonId . flip (^.) id

instance ReadOnlyDatabase Person PersonId PersonRowId where
  valueAndRowId = valueAndRowIdImpl
  getValueByRowId = getValueByRowIdImpl
instance ReadWriteDatabase Person PersonId PersonRowId where
  forceInsert = insert . entityToRow

class PersonRowable a where
  toPersonRowId :: a -> DbCall PersonRowId

instance PersonRowable PersonRowId where toPersonRowId = return
instance PersonRowable Person where toPersonRowId = insertOrVerify
