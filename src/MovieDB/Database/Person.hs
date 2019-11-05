{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.Person(
  init,
  PersonRowId,
  PersonRowable,
) where

import Prelude                          hiding (id, init)

import Data.Text                        (Text)

import Control.Lens                     (classUnderscoreNoPrefixFields, makeLensesWith, (^.))

import MovieDB.Database                 (DbCall)
import MovieDB.Database.Internal.Common (RowIso(..), ToKey, runInit)
import MovieDB.Types                    (Person(Person), PersonId, mkPersonId)

import Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
PersonRow sql=person
  personId        Text
  UniquePersonId  personId
  name            Text
|]

init :: DbCall ()
init = runInit migrateTables


makeLensesWith classUnderscoreNoPrefixFields ''PersonId
makeLensesWith classUnderscoreNoPrefixFields ''Person

instance RowIso Person PersonId PersonRow where
  extractId = flip (^.) id
  unique = UniquePersonId . flip (^.) id
  entityToRow person = PersonRow (person ^. id ^. id) (person ^. name)
  rowToEntity (PersonRow id name) = Person (mkPersonId id) name

type PersonRowable m = ToKey m PersonRow
