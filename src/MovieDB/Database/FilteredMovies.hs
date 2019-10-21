{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module MovieDB.Database.FilteredMovies(
  init,
  clear,
  addFilteredMovie,
  removeFilteredMovie,
  isFiltered,
  isNotFiltered,
  allFilteredMovies,
) where

import Prelude                    hiding (id, init)

import MovieDB.Database.Common    (DbCall, DbPath(..), getValueByRowId)
import MovieDB.Database.Movies    (MovieRowId, MovieRowable, toMovieRowId)
import MovieDB.Database.TypesTH   ()
import MovieDB.Types              (FilterReason, FilteredMovie(..))

import Control.Monad              ((>=>))
import Control.Monad.Trans.Reader (ask)
import Data.Maybe                 (isJust)

import Common.Operators

import Database.Persist.Sql       (Filter, deleteBy, deleteWhere, entityVal, getBy, insert, selectList)
import Database.Persist.Sqlite    (runMigrationSilent, runSqlite)
import Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
FilteredMovieRow
  movieId         MovieRowId
  reason          FilterReason
  UniqueMovieId   movieId
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)

init :: DbCall()
init = withMigration $ return ()

passFilter = [] :: [Filter FilteredMovieRow]

clear :: DbCall()
clear = withMigration $ deleteWhere passFilter

instance MovieRowable FilteredMovie where
  toMovieRowId = toMovieRowId . _movie

addFilteredMovie :: FilteredMovie -> DbCall FilteredMovieRowId
addFilteredMovie = toRow >=> (withMigration . insert) where
  toRow :: FilteredMovie -> DbCall FilteredMovieRow
  toRow fm = FilteredMovieRow <$> toMovieRowId fm <*> return (_reason fm)

removeFilteredMovie :: FilteredMovie -> DbCall ()
removeFilteredMovie = toMovieRowId >=> (withMigration . deleteBy . UniqueMovieId)

isFiltered :: MovieRowable m => m -> DbCall Bool
isFiltered = toMovieRowId >=> (fmap isJust . withMigration . getBy . UniqueMovieId)

isNotFiltered :: MovieRowable m => m -> DbCall Bool
isNotFiltered = not <$< isFiltered

allFilteredMovies :: DbCall [FilteredMovie]
allFilteredMovies = do
  rows <- fmap entityVal <$> withMigration (selectList passFilter [])
  let ids = map filteredMovieRowMovieId rows
  let reasons = map filteredMovieRowReason rows
  movies <- traverse getValueByRowId ids
  return $ zipWith FilteredMovie movies reasons
