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

module MovieDB.Database.FilteredMovie(
  init,
  clear,
  addFilteredMovie,
  removeFilteredMovie,
  isFiltered,
  isNotFiltered,
  allFilteredMovies,
) where

import           Prelude                           hiding (id, init)

import           Data.Maybe                        (isJust)
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector (fromList)

import           Control.Monad                     ((>=>))
import           Data.Functor                      (void)

import           MovieDB.Database                  (DbCall)
import           MovieDB.Database.Internal.Common  (getValueByRowId, ToKey(..))
import           MovieDB.Database.Internal.TypesTH ()
import           MovieDB.Database.Movie            (MovieRowId, MovieRowable, MovieRow)
import           MovieDB.Types                     (FilterReason, FilteredMovie(..))

import           Common.Operators

import           Database.Persist.Sql              (Filter, deleteBy, deleteWhere, entityVal, getBy, insert, runMigrationSilent, selectList)
import           Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


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

instance {-# OVERLAPPING #-} ToKey FilteredMovie MovieRow where
  getKeyFor = getKeyFor . _movie

addFilteredMovie :: FilteredMovie -> DbCall FilteredMovieRowId
addFilteredMovie = toRow >=> insert where
  toRow :: FilteredMovie -> DbCall FilteredMovieRow
  toRow fm = FilteredMovieRow <$> getKeyFor fm <*> return (_reason fm)

removeFilteredMovie :: FilteredMovie -> DbCall ()
removeFilteredMovie = getKeyFor >=> (deleteBy . UniqueMovieId)

isFiltered :: MovieRowable m => m -> DbCall Bool
isFiltered = getKeyFor >=> (fmap isJust . getBy . UniqueMovieId)

isNotFiltered :: MovieRowable m => m -> DbCall Bool
isNotFiltered = not <$< isFiltered

allFilteredMovies :: DbCall (Vector FilteredMovie)
allFilteredMovies = do
  rows <- fmap entityVal <$> selectList passFilter []
  let ids = map filteredMovieRowMovieId rows
  let reasons = map filteredMovieRowReason rows
  movies <- traverse getValueByRowId ids
  return $ Vector.fromList $ zipWith FilteredMovie movies reasons
