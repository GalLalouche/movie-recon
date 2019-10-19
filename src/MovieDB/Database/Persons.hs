{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                                                     #-}


module MovieDB.Database.Persons(
  init,
  clear,
  PersonRowId,
  PersonRowable,
  toPersonRowId,
) where

import Prelude                    hiding (id, init)

import Common.Operators

import MovieDB.Database.Common    (DbCall(..), DbPath(..), ExtractableId(..), ReadOnlyDatabase(..),
                                   ReadWriteDatabase(..), insertOrVerify)
import MovieDB.Types              (Person(..), PersonId(..))

import Control.Arrow              ((&&&))
import Control.Lens               (Iso', classUnderscoreNoPrefixFields, from, iso, makeLensesWith, view, (^.))
import Control.Monad.Trans.Maybe  (MaybeT(..))
import Control.Monad.Trans.Reader (ask)
import Data.Maybe                 (fromJust)
import Data.Text                  (Text)

import Database.Persist.Sql       (Filter, deleteWhere, entityKey, entityVal, get, getBy, insert)
import Database.Persist.Sqlite    (runMigrationSilent, runSqlite)
import Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
PersonRow
  personId        Text
  UniquePersonId  personId
  name            Text
|]


makeLensesWith classUnderscoreNoPrefixFields ''PersonId
makeLensesWith classUnderscoreNoPrefixFields ''Person

rowIso :: Iso' Person PersonRow
rowIso = iso fromPerson toPerson where
  fromPerson :: Person -> PersonRow
  fromPerson person = PersonRow (person ^. id ^. id) (person ^. name)
  toPerson :: PersonRow -> Person
  toPerson (PersonRow id name) = Person (PersonId id) name

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter PersonRow])

invertIso :: PersonRow -> Person
invertIso = view (from rowIso)

instance ExtractableId Person PersonId where
  extractId = view id
instance ReadOnlyDatabase Person PersonId PersonRowId where
  -- TODO handle code duplication
  valueAndRowId personId = MaybeT $ withMigration $ do
    result <- getBy $ UniquePersonId $ personId ^. id
    return $  result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId personRowId = withMigration $ invertIso . fromJust <$> get personRowId
instance ReadWriteDatabase Person PersonId PersonRowId where
  forceInsert = withMigration . insert . view rowIso

class PersonRowable a where
  toPersonRowId :: a -> DbCall PersonRowId

instance PersonRowable PersonRowId where toPersonRowId = return
instance PersonRowable Person where toPersonRowId = insertOrVerify

