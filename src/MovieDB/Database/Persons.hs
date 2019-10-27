{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.Persons(
  init,
  clear,
  PersonRowId,
  PersonRowable,
  toPersonRowId,
) where

import Prelude                    hiding (id, init)

import Data.Maybe                 (fromJust)
import Data.Text                  (Text)

import Control.Arrow              ((&&&))
import Control.Lens               (Iso', classUnderscoreNoPrefixFields, from, iso, makeLensesWith, view, (^.))
import Data.Functor (void)
import Control.Monad.Trans.Maybe  (MaybeT(..))

import MovieDB.Database.Common    (DbCall, ExtractableId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), insertOrVerify)
import MovieDB.Types              (Person(..), PersonId, mkPersonId)

import Database.Persist.Sql       (Filter, deleteWhere, entityKey, entityVal, get, getBy, insert)
import Database.Persist.Sqlite    (runMigrationSilent)
import Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Common.Operators


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
PersonRow sql=person
  personId        Text
  UniquePersonId  personId
  name            Text
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

clear :: DbCall()
clear = deleteWhere ([] :: [Filter PersonRow])

makeLensesWith classUnderscoreNoPrefixFields ''PersonId
makeLensesWith classUnderscoreNoPrefixFields ''Person

rowIso :: Iso' Person PersonRow
rowIso = iso fromPerson toPerson where
  fromPerson :: Person -> PersonRow
  fromPerson person = PersonRow (person ^. id ^. id) (person ^. name)
  toPerson :: PersonRow -> Person
  toPerson (PersonRow id name) = Person (mkPersonId id) name

invertIso :: PersonRow -> Person
invertIso = view (from rowIso)


instance ExtractableId Person PersonId where
  extractId = view id
instance ReadOnlyDatabase Person PersonId PersonRowId where
  -- TODO handle code duplication
  valueAndRowId personId = MaybeT $ do
    result <- getBy $ UniquePersonId $ personId ^. id
    return $  result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId personRowId = invertIso . fromJust <$> get personRowId
instance ReadWriteDatabase Person PersonId PersonRowId where
  forceInsert = insert . view rowIso

class PersonRowable a where
  toPersonRowId :: a -> DbCall PersonRowId

instance PersonRowable PersonRowId where toPersonRowId = return
instance PersonRowable Person where toPersonRowId = insertOrVerify
