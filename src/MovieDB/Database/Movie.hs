{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module MovieDB.Database.Movie(
  init,
  clear,
  getValue,
  insertOrVerify,
  allMovies,
  MovieRow,
  MovieRowId,
  MovieRowable,
  MaybeMovieRowable,
  toMaybeMovieRowId,
) where

import           Prelude                          hiding (id, init)

import           Data.Text                        (Text)
import           Data.Time                        (Day)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector (fromList)

import           Control.Lens                     (classUnderscoreNoPrefixFields, makeLensesWith, (^.))
import           Control.Monad.Trans.Class        (lift)
import           Data.Functor                     (void)

import           MovieDB.Database                 (DbCall, DbMaybe)
import           MovieDB.Database.Internal.Common (ReadWriteDatabase(..), RowIso(..), ToKey(..), getRowId, getValue, insertOrVerify)
import           MovieDB.Types                    (Movie(..), MovieId, mkMovieId)

import           Database.Persist.Sql             (Filter, deleteWhere, entityVal, insert, runMigrationSilent, selectList)
import           Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


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
  unique movieId = UniqueMovieId $ movieId ^. id
  entityToRow movie = MovieRow (movie ^. id ^. id) (movie ^. name) (movie ^. date)
  rowToEntity (MovieRow id name date) = Movie (mkMovieId id) name date

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

passFilter = [] :: [Filter MovieRow]

clear :: DbCall ()
clear = deleteWhere passFilter

allMovies :: DbCall (Vector Movie)
allMovies = Vector.fromList . fmap (rowToEntity . entityVal) <$> selectList passFilter []

instance ReadWriteDatabase Movie MovieId MovieRow where
  forceInsert = insert . entityToRow

class MaybeMovieRowable a where
  toMaybeMovieRowId :: a -> DbMaybe MovieRowId

type MovieRowable m = ToKey m MovieRow

instance MovieRowable m => MaybeMovieRowable m where toMaybeMovieRowId m = lift $ getKeyFor m
instance MaybeMovieRowable MovieId where toMaybeMovieRowId = getRowId
