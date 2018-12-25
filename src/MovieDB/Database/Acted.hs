{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Acted(
  init,
  clear,
) where

import Prelude hiding (init)

import Common.Operators

import MovieDB.Types (Actor, ActorId)
import MovieDB.Database.Common (DbCall, path)
import MovieDB.Database.Participations (ParticipationReadOnly(..), ParticipationDatabase(..))
import MovieDB.Database.Movies (MovieRowId)
import MovieDB.Database.Actors (ActorRowId)

import Control.Monad.Trans.Reader (ask)

import Database.Persist.Sql (deleteWhere, selectList, (==.), Entity, entityVal, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Acted
  movieId         MovieRowId
  actorId         ActorRowId
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter Acted])


entities idFunction column rowId = do
    result <- withMigration $ selectList [column ==. rowId] []
    return $ (idFunction . entityVal) <$> result

instance ParticipationReadOnly ActorRowId where
  crewRowIds = entities actedActorId ActedMovieId
  movieRowIds = entities actedMovieId ActedActorId

instance ParticipationDatabase ActorRowId ActedId where
  addEntry = withMigration . insert .: Acted
