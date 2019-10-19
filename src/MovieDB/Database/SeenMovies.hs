{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.SeenMovies(
  init,
  clear,
  addSeenMovie,
  isSeen,
  isNotSeen,
  removeSeenMovie,
) where

import Prelude                    hiding (init)

import Common.Operators

import MovieDB.Database.Common    (DbCall, getValueByRowId, path)
import MovieDB.Database.Movies    (MovieRowId, MovieRowable, toMovieRowId)
import MovieDB.Types              (Movie)

import Control.Monad              ((>=>))
import Control.Monad.Trans.Reader (ask)
import Data.Maybe                 (isJust)

import Database.Persist.Sql       (Filter, deleteBy, deleteWhere, entityVal, getBy, insert, selectList)
import Database.Persist.Sqlite    (runMigrationSilent, runSqlite)
import Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
SeenMovies
  movieId         MovieRowId
  UniqueMovieId   movieId
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

passFilter = [] :: [Filter SeenMovies]

clear :: DbCall()
clear = withMigration $ deleteWhere passFilter

addSeenMovie :: MovieRowable m => m -> DbCall SeenMoviesId
addSeenMovie = toMovieRowId >=> (withMigration . insert . SeenMovies)

removeSeenMovie :: MovieRowable m => m -> DbCall ()
removeSeenMovie = toMovieRowId >=> (withMigration . deleteBy . UniqueMovieId)

isSeen :: MovieRowable m => m -> DbCall Bool
isSeen = toMovieRowId >=> (fmap isJust . withMigration . getBy . UniqueMovieId)

isNotSeen :: MovieRowable m => m -> DbCall Bool
isNotSeen = not <$< isSeen

allSeenMovies :: DbCall [Movie]
allSeenMovies = do
  ids <- fmap (seenMoviesMovieId . entityVal) <$> withMigration (selectList passFilter [])
  traverse getValueByRowId ids
