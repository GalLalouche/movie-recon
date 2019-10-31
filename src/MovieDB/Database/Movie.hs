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
  MovieRowId,
  MovieRowable,
  toMovieRowId,
  MaybeMovieRowable,
  toMaybeMovieRowId,
) where

import           Prelude                          hiding (id, init)

import           Data.Text                        (Text)
import           Data.Time                        (Day)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector (fromList)

import           Control.Lens                     (Iso', classUnderscoreNoPrefixFields, from, iso, makeLensesWith, view, (^.))
import           Control.Monad.Trans.Class        (lift)
import           Data.Functor                     (void)

import           MovieDB.Database                 (DbCall, DbMaybe)
import           MovieDB.Database.Internal.Common (ExtractableId(..), GetUniqueId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), RowIso(..), getValue, getValueByRowId, getValueByRowIdImpl, insertOrVerify, valueAndRowIdImpl)
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

instance RowIso Movie MovieRow where
  entityToRow movie = MovieRow (movie ^. id ^. id) (movie ^. name) (movie ^. date)
  rowToEntity (MovieRow id name date) = Movie (mkMovieId id) name date

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

passFilter = [] :: [Filter MovieRow]

clear :: DbCall ()
clear = deleteWhere passFilter

allMovies :: DbCall (Vector Movie)
allMovies = Vector.fromList . fmap (rowToEntity . entityVal) <$> selectList passFilter []

instance ExtractableId Movie MovieId where extractId = view id
instance GetUniqueId MovieId MovieRow where unique = UniqueMovieId . flip (^.) id
instance ReadOnlyDatabase Movie MovieId MovieRowId where
  valueAndRowId = valueAndRowIdImpl
  getValueByRowId = getValueByRowIdImpl
instance ReadWriteDatabase Movie MovieId MovieRowId where
  forceInsert = insert . entityToRow

class MovieRowable a where
  toMovieRowId :: a -> DbCall MovieRowId

instance MovieRowable MovieRowId where toMovieRowId = return
instance MovieRowable Movie where toMovieRowId = insertOrVerify

class MaybeMovieRowable a where
  toMaybeMovieRowId :: a -> DbMaybe MovieRowId
instance MovieRowable m => MaybeMovieRowable m where toMaybeMovieRowId m = lift $ toMovieRowId m
instance MaybeMovieRowable MovieId where toMaybeMovieRowId m = fst <$> valueAndRowId m
