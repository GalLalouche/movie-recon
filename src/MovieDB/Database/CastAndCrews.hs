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
import MovieDB.Database.Common (DbPath(..), DbCall(..), DbMaybe(..), ExtractableId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), insertOrVerify, getRowId, getValueByRowId, insertOrVerify, getValue)
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
import Data.Maybe (fromJust)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
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

rowIdByMovieId :: T.MovieId -> DbMaybe CastAndCrewRowId
rowIdByMovieId movieId = do
  movieRowId <- getRowId movieId :: DbMaybe MovieRowId
  row <- MaybeT $ withMigration $ getBy $ UniqueMovieId movieRowId :: DbMaybe (Entity CastAndCrewRow)
  return $ entityKey row

instance ReadOnlyDatabase CastAndCrew CastAndCrewId CastAndCrewRowId where
  getValueByRowId rowId = do
    (CastAndCrewRow movieRowId directorRowId writerRowId actorRowIds) <- withMigration $ fromJust <$> get rowId
    movie <- getValueByRowId movieRowId
    director <- getValueByRowId directorRowId
    writer <- getValueByRowId writerRowId
    actors <- traverse getValueByRowId actorRowIds
    return $ CastAndCrew movie director writer actors
  valueAndRowId (CastAndCrewId id) = do
    let movieId = id :: T.MovieId
    rowId <- rowIdByMovieId movieId
    value <- MaybeT $ Just <$> getValueByRowId rowId
    return (rowId, value)

instance ReadWriteDatabase CastAndCrew CastAndCrewId CastAndCrewRowId where
  forceInsert (CastAndCrew movie director writer actors) = do
    row <- CastAndCrewRow <$>
      insertOrVerify movie <*>
      insertOrVerify director <*>
      insertOrVerify writer<*>
      traverse insertOrVerify actors
    withMigration $ insert row

