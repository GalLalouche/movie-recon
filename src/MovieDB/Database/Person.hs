{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
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
) where

import Prelude                          hiding (id, init)

import Data.Text                        (Text)

import Control.Lens                     (classUnderscoreNoPrefixFields, makeLensesWith, (^.))
import Data.Functor                     (void)

import MovieDB.Database                 (DbCall)
import MovieDB.Database.Internal.Common (ReadWriteDatabase(..), RowIso(..), ToKey)
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

instance RowIso Person PersonId PersonRow where
  extractId = flip (^.) id
  unique personId = UniquePersonId $ personId ^. id
  entityToRow person = PersonRow (person ^. id ^. id) (person ^. name)
  rowToEntity (PersonRow id name) = Person (mkPersonId id) name

instance ReadWriteDatabase Person PersonId PersonRow where
  forceInsert person = insert $ entityToRow person

type PersonRowable m = ToKey m PersonRow
