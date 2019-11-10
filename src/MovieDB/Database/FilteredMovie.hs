{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module MovieDB.Database.FilteredMovie(
  init,
  addFilteredMovie,
  isFiltered,
  isNotFiltered,
  allFilteredMovies,
) where

import           Prelude                           hiding (id, init)

import           Data.Maybe                        (isJust)
import           Data.Vector                       (Vector)
import qualified Data.Vector                       as Vector (fromList)

import           Control.Monad                     ((>=>))

import           MovieDB.Database                  (DbCall)
import           MovieDB.Database.Internal.Common  (ToKey(..), getValueByRowId, runInit)
import           MovieDB.Database.Internal.TypesTH ()
import           MovieDB.Database.Movie            (MovieRow, MovieRowId, MovieRowable)
import           MovieDB.Types                     (FilterReason, FilteredMovie(FilteredMovie, _movie, _reason))

import           Database.Persist.Sql              (entityVal, getBy, insert, selectList)
import           Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import           Common.Arrows                     (uncurryA2)
import           Common.Operators                  ((<$<), (<*$>))


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
FilteredMovieRow sql=filtered_movie
  movieId         MovieRowId
  reason          FilterReason
  UniqueMovieId   movieId
|]

init :: DbCall ()
init = runInit migrateTables


instance {-# OVERLAPPING #-} ToKey FilteredMovie MovieRow where
  getKeyFor = getKeyFor . _movie

addFilteredMovie :: FilteredMovie -> DbCall FilteredMovieRowId
addFilteredMovie = toRow >=> insert where
  toRow :: FilteredMovie -> DbCall FilteredMovieRow
  toRow = uncurryA2 FilteredMovieRow getKeyFor (return . _reason)

isFiltered :: MovieRowable m => m -> DbCall Bool
isFiltered = getKeyFor >=> fmap isJust . getBy . UniqueMovieId

isNotFiltered :: MovieRowable m => m -> DbCall Bool
isNotFiltered = not <$< isFiltered

allFilteredMovies :: DbCall (Vector FilteredMovie)
allFilteredMovies = do
  rows <- fmap entityVal <$> selectList [] []
  let ids = map filteredMovieRowMovieId rows
  movies <- traverse getValueByRowId ids
  let reasons = map filteredMovieRowReason rows
  return $ Vector.fromList $ zipWith FilteredMovie movies reasons
