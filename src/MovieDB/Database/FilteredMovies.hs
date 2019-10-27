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

import Prelude                           hiding (id, init)

import Data.Maybe                        (isJust)

import Control.Monad                     ((>=>))
import Data.Functor                      (void)

import MovieDB.Database                  (DbCall)
import MovieDB.Database.Internal.Common  (getValueByRowId)
import MovieDB.Database.Internal.TypesTH ()
import MovieDB.Database.Movies           (MovieRowId, MovieRowable, toMovieRowId)
import MovieDB.Types                     (FilterReason, FilteredMovie(..))

import Common.Operators

import Database.Persist.Sql              (Filter, deleteBy, deleteWhere, entityVal, getBy, insert, selectList)
import Database.Persist.Sqlite           (runMigrationSilent)
import Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
FilteredMovieRow sql=filtered_movie
  movieId         MovieRowId
  reason          FilterReason
  UniqueMovieId   movieId
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

passFilter = [] :: [Filter FilteredMovieRow]

clear :: DbCall ()
clear = deleteWhere passFilter


instance MovieRowable FilteredMovie where
  toMovieRowId = toMovieRowId . _movie

addFilteredMovie :: FilteredMovie -> DbCall FilteredMovieRowId
addFilteredMovie = toRow >=> insert where
  toRow :: FilteredMovie -> DbCall FilteredMovieRow
  toRow fm = FilteredMovieRow <$> toMovieRowId fm <*> return (_reason fm)

removeFilteredMovie :: FilteredMovie -> DbCall ()
removeFilteredMovie = toMovieRowId >=> (deleteBy . UniqueMovieId)

isFiltered :: MovieRowable m => m -> DbCall Bool
isFiltered = toMovieRowId >=> (fmap isJust . getBy . UniqueMovieId)

isNotFiltered :: MovieRowable m => m -> DbCall Bool
isNotFiltered = not <$< isFiltered

allFilteredMovies :: DbCall [FilteredMovie]
allFilteredMovies = do
  rows <- fmap entityVal <$> selectList passFilter []
  let ids = map filteredMovieRowMovieId rows
  let reasons = map filteredMovieRowReason rows
  movies <- traverse getValueByRowId ids
  return $ zipWith FilteredMovie movies reasons
