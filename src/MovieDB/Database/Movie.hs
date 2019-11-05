{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.Movie(
  init,
  getValue,
  getAll,
  MovieRow,
  MovieRowId,
  MovieRowable,
) where

import Prelude                          hiding (id, init)

import Data.Text                        (Text)
import Data.Time                        (Day)

import Control.Lens                     (classUnderscoreNoPrefixFields, makeLensesWith, (^.))

import MovieDB.Database                 (DbCall)
import MovieDB.Database.Internal.Common (RowIso(..), ToKey(..), getAll, getValue, runInit)
import MovieDB.Types                    (Movie(..), MovieId, mkMovieId)

import Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MovieRow sql=movie
  movieId         Text
  UniqueMovieId   movieId
  name            Text
  date            Day
|]

makeLensesWith classUnderscoreNoPrefixFields ''MovieId
makeLensesWith classUnderscoreNoPrefixFields ''Movie

instance RowIso Movie MovieId MovieRow where
  extractId = flip (^.) id
  unique = UniqueMovieId . flip (^.) id
  entityToRow movie = MovieRow (movie ^. id ^. id) (movie ^. name) (movie ^. date)
  rowToEntity (MovieRow id name date) = Movie (mkMovieId id) name date

init :: DbCall ()
init = runInit migrateTables

type MovieRowable m = ToKey m MovieRow
