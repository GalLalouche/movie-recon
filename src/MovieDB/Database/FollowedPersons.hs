{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.FollowedPersons(
  init,
  clear,
  addFollowedPerson,
  removeFollowedPerson,
  isFollowed,
  allFollowedPersons
) where

import Prelude                          hiding (init)

import Data.Maybe                       (isJust)

import Control.Monad                    ((>=>))
import Data.Functor                     (void)

import MovieDB.Database                 (DbCall)
import MovieDB.Database.Internal.Common (getValueByRowId)
import MovieDB.Database.Persons         (PersonRowId, PersonRowable, toPersonRowId)
import MovieDB.Types                    (Person)

import Database.Persist.Sql             (Filter, deleteBy, deleteWhere, entityVal, getBy, insert, selectList)
import Database.Persist.Sqlite          (runMigrationSilent)
import Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
FollowedPerson
  personId        PersonRowId
  UniquePersonId  personId
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

passFilter = [] :: [Filter FollowedPerson]

clear :: DbCall ()
clear = deleteWhere passFilter

addFollowedPerson :: PersonRowable m => m -> DbCall FollowedPersonId
addFollowedPerson = toPersonRowId >=> (insert . FollowedPerson)

removeFollowedPerson :: PersonRowable m => m -> DbCall ()
removeFollowedPerson = toPersonRowId >=> (deleteBy . UniquePersonId)

isFollowed :: PersonRowable m => m -> DbCall Bool
isFollowed = toPersonRowId >=> (fmap isJust . getBy . UniquePersonId)

allFollowedPersons :: DbCall [Person]
allFollowedPersons = do
  ids <- fmap (followedPersonPersonId . entityVal) <$> selectList passFilter []
  traverse getValueByRowId ids
