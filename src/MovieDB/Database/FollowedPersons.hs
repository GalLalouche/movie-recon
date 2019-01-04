{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                                                           #-}

module MovieDB.Database.FollowedPersons(
  init,
  clear,
  addFollowedPerson,
  removeFollowedPerson,
) where

import Prelude                    hiding (init)

import Common.Operators

import MovieDB.Database.Common    (DbCall, path)
import MovieDB.Database.Persons   (PersonRowId, PersonRowable, toPersonRowId)

import Control.Monad              ((>=>))
import Control.Monad.Trans.Reader (ask)

import Database.Persist.Sql       (Filter, deleteBy, deleteWhere, insert)
import Database.Persist.Sqlite    (runMigrationSilent, runSqlite)
import Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
FollowedPersons
  personId        PersonRowId
  UniquePersonId  personId
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter FollowedPersons])

addFollowedPerson :: PersonRowable m => m -> DbCall FollowedPersonsId
addFollowedPerson = toPersonRowId >=> (withMigration . insert . FollowedPersons)

removeFollowedPerson :: PersonRowable m => m -> DbCall ()
removeFollowedPerson = toPersonRowId >=> (withMigration . deleteBy . UniquePersonId)
