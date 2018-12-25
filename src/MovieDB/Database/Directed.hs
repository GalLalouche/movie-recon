{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Directed(
  init,
  clear,
) where

import Prelude hiding (init)

import Common.Operators

import MovieDB.Types (Director, DirectorId)
import MovieDB.Database.Common (DbCall, path)
import MovieDB.Database.Participations (ParticipationReadOnly(..), ParticipationDatabase(..))
import MovieDB.Database.Movies (MovieRowId)
import MovieDB.Database.Directors (DirectorRowId)

import Control.Monad.Trans.Reader (ask)

import Database.Persist.Sql (deleteWhere, selectList, (==.), Entity, entityVal, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Directed
  movieId            MovieRowId
  directorId         DirectorRowId
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter Directed])


entities idFunction column rowId = do
    result <- withMigration $ selectList [column ==. rowId] []
    return $ (idFunction . entityVal) <$> result

instance ParticipationReadOnly DirectorRowId where
  crewRowIds = entities directedDirectorId DirectedMovieId
  movieRowIds = entities directedMovieId DirectedDirectorId

instance ParticipationDatabase DirectorRowId DirectedId where
  addEntry = withMigration . insert .: Directed
