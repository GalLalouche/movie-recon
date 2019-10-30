{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.FollowedPerson(
  init,
  clear,
  addFollowedPerson,
  isFollowed,
  allFollowedPersons
) where

import           Prelude                          hiding (init)

import           Data.Maybe                       (isJust)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector (fromList)

import           Control.Monad                    ((>=>))
import           Data.Functor                     (void)

import           MovieDB.Database                 (DbCall)
import           MovieDB.Database.Internal.Common (getValueByRowId)
import           MovieDB.Database.Person          (PersonRowId, PersonRowable, toPersonRowId)
import           MovieDB.Types                    (ParticipationType(Actor), Person)

import           Database.Persist.Sql             (Filter, deleteBy, deleteWhere, entityVal, getBy, insert, runMigrationSilent, selectList)
import           Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import           Common.Operators


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
FollowedPerson
  personId        PersonRowId
  ignoreActing    Bool
  UniquePersonId  personId
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

passFilter = [] :: [Filter FollowedPerson]

clear :: DbCall ()
clear = deleteWhere passFilter

addFollowedPerson :: PersonRowable p => Bool -> p -> DbCall FollowedPersonId
addFollowedPerson ignoreActing = toPersonRowId >=> (insert . flip FollowedPerson ignoreActing)

isFollowed :: PersonRowable m => ParticipationType -> m -> DbCall Bool
isFollowed pt = toPersonRowId >=> getBy . UniquePersonId >$> any (aux . entityVal) where
  aux (FollowedPerson _ ignoreActing) = pt /= Actor || not ignoreActing

allFollowedPersons :: DbCall (Vector Person)
allFollowedPersons = do
  ids <- Vector.fromList . fmap (followedPersonPersonId . entityVal) <$> selectList passFilter []
  traverse getValueByRowId ids
