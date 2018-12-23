{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.CastAndCrews(
  init,
  clear,
  insertOrVerify,
  getValue,
) where

import Prelude hiding (init, id)

import Common.Operators

import MovieDB.Types (CastAndCrew(..), CastAndCrewIds(..), CastAndCrewId(..))
import MovieDB.Database.Common (DbPath(..), DbCall(..), DbMaybe(..), ExtractableId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), insertOrVerify, getKey, getValueByRowId, insertOrVerify, getValue)
import qualified MovieDB.Types as T
import MovieDB.Database.Movies (MovieRowId)
import qualified MovieDB.Database.Movies as MDB
import MovieDB.Database.Directors (DirectorRowId)
import qualified MovieDB.Database.Directors as DDB
import MovieDB.Database.Actors (ActorRowId)
import qualified MovieDB.Database.Actors as ADB
import MovieDB.Database.Writers (WriterRowId)
import qualified MovieDB.Database.Writers as WDB

import Data.Text (Text)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Control.Lens (makeLensesWith, classUnderscoreNoPrefixFields, Iso', iso, from, view, (^.))

import Database.Persist.Sql (deleteWhere, Entity, entityKey, entityVal, get, getBy, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
CastAndCrewRow
  movieId            MovieRowId
  UniqueMovieId      movieId
  directorId         DirectorRowId
  writerId           WriterRowId
  actorIds           [ActorRowId]
|]

makeLensesWith classUnderscoreNoPrefixFields ''T.MovieId
makeLensesWith classUnderscoreNoPrefixFields ''T.Movie
makeLensesWith classUnderscoreNoPrefixFields ''T.CastAndCrew

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter CastAndCrewRow])

instance ExtractableId CastAndCrew CastAndCrewId where
  extractId c = CastAndCrewId $ c ^. movie ^. id

aux :: T.MovieId -> DbMaybe CastAndCrewRowId
aux movieId = do
  movieRowId <- getKey movieId :: DbMaybe MovieRowId
  row <- MaybeT $ withMigration $ getBy $ UniqueMovieId movieRowId :: DbMaybe (Entity CastAndCrewRow)
  return $ entityKey row

instance ReadOnlyDatabase CastAndCrew CastAndCrewId CastAndCrewRowId where
  getValueByRowId rowId = do
    (CastAndCrewRow movieRowId directorRowId writerRowId actorRowIds) <- MaybeT $ withMigration $ get rowId
    movie <- getValueByRowId movieRowId
    director <- getValueByRowId directorRowId
    writer <- getValueByRowId writerRowId
    actors <- traverse getValueByRowId actorRowIds
    return $ CastAndCrew movie director writer actors
  keyVal (CastAndCrewId id) = do
    let movieId = id :: T.MovieId
    rowId <- aux movieId
    value <- getValueByRowId rowId
    return (rowId, value)

instance ReadWriteDatabase CastAndCrew CastAndCrewId CastAndCrewRowId where
  forceInsert (CastAndCrew movie director writer actors) = do
    row <- CastAndCrewRow <$>
      insertOrVerify movie <*>
      insertOrVerify director <*>
      insertOrVerify writer<*>
      traverse insertOrVerify actors
    withMigration $ insert row

