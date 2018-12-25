{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Wrote(
  init,
  clear,
) where

import Prelude hiding (init)

import Common.Operators

import MovieDB.Types (Writer, WriterId)
import MovieDB.Database.Common (DbCall, path)
import MovieDB.Database.Participations (ParticipationReadOnly(..), ParticipationDatabase(..))
import MovieDB.Database.Movies (MovieRowId)
import MovieDB.Database.Writers (WriterRowId)

import Control.Monad.Trans.Reader (ask)

import Database.Persist.Sql (deleteWhere, selectList, (==.), Entity, entityVal, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Wrote
  movieId          MovieRowId
  writerId         WriterRowId
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter Wrote])


entities idFunction column rowId = do
    result <- withMigration $ selectList [column ==. rowId] []
    return $ (idFunction . entityVal) <$> result

instance ParticipationReadOnly WriterRowId where
  crewRowIds = entities wroteWriterId WroteMovieId
  movieRowIds = entities wroteMovieId WroteWriterId

instance ParticipationDatabase WriterRowId WroteId where
  addEntry = withMigration . insert .: Wrote
