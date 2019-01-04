{-# LANGUAGE FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                                                           #-}

module MovieDB.Database.FollowedPersons(
  init,
  clear,
  addFollowedPerson,
  removeFollowedPerson,
  isFollowed,
  allFollowedPersons
) where

import Prelude                    hiding (init)

import Common.Operators

import MovieDB.Database.Common    (DbCall, getValueByRowId, path)
import MovieDB.Database.Persons   (PersonRowId, PersonRowable, toPersonRowId)
import MovieDB.Types              (Person)

import Control.Monad              ((>=>))
import Control.Monad.Trans.Reader (ask)
import Data.Maybe                 (isJust)

import Database.Persist.Sql       (Filter, deleteBy, deleteWhere, entityVal, getBy, insert, selectList)
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

passFilter = [] :: [Filter FollowedPersons]
clear :: DbCall()
clear = withMigration $ deleteWhere passFilter

addFollowedPerson :: PersonRowable m => m -> DbCall FollowedPersonsId
addFollowedPerson = toPersonRowId >=> (withMigration . insert . FollowedPersons)

removeFollowedPerson :: PersonRowable m => m -> DbCall ()
removeFollowedPerson = toPersonRowId >=> (withMigration . deleteBy . UniquePersonId)

isFollowed :: PersonRowable m => m -> DbCall Bool
isFollowed = toPersonRowId >=> (fmap isJust . withMigration . getBy . UniquePersonId)

allFollowedPersons :: DbCall [Person]
allFollowedPersons = do
  ids <- fmap (followedPersonsPersonId . entityVal) <$> withMigration (selectList passFilter [])
  traverse getValueByRowId ids
