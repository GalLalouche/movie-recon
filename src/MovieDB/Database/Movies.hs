{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module MovieDB.Database.Movies(
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

import           Data.Maybe                       (fromJust)
import           Data.Text                        (Text)
import           Data.Time                        (Day)
import           Data.Vector                      (Vector)
import qualified Data.Vector                      as Vector (fromList)

import           Control.Arrow                    ((&&&))
import           Control.Lens                     (Iso', classUnderscoreNoPrefixFields, from, iso, makeLensesWith, view, (^.))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Maybe        (MaybeT(..))
import           Data.Functor                     (void)

import           MovieDB.Database                 (DbCall, DbMaybe)
import           MovieDB.Database.Internal.Common (ExtractableId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), getValue, getValueByRowId, insertOrVerify)
import           MovieDB.Types                    (Movie(..), MovieId, mkMovieId)

import           Database.Persist.Sql             (Filter, deleteWhere, entityKey, entityVal, get, getBy, insert, runMigrationSilent, selectList)
import           Database.Persist.TH              (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import           Common.Operators


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MovieRow sql=movie
  movieId         Text
  UniqueMovieId   movieId
  name            Text
  date            Day
|]

makeLensesWith classUnderscoreNoPrefixFields ''MovieId
makeLensesWith classUnderscoreNoPrefixFields ''Movie

rowIso :: Iso' Movie MovieRow
rowIso = iso fromMovie toMovie where
  fromMovie :: Movie -> MovieRow
  fromMovie movie = MovieRow (movie ^. id ^. id) (movie ^. name) (movie ^. date)
  toMovie :: MovieRow -> Movie
  toMovie (MovieRow id name date) = Movie (mkMovieId id) name date

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

passFilter = [] :: [Filter MovieRow]

clear :: DbCall ()
clear = deleteWhere passFilter


invertIso :: MovieRow -> Movie
invertIso = view $ from rowIso

allMovies :: DbCall (Vector Movie)
allMovies = Vector.fromList . fmap (invertIso . entityVal) <$> selectList passFilter []

instance ExtractableId Movie MovieId where
  extractId = view id
instance ReadOnlyDatabase Movie MovieId MovieRowId where
  -- TODO handle code duplication
  valueAndRowId movieId = MaybeT $ do
    result <- getBy $ UniqueMovieId $ movieId ^. id
    return $ result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId movieRowId = invertIso . fromJust <$> get movieRowId
instance ReadWriteDatabase Movie MovieId MovieRowId where
  forceInsert = insert . view rowIso

class MovieRowable a where
  toMovieRowId :: a -> DbCall MovieRowId

instance MovieRowable MovieRowId where toMovieRowId = return
instance MovieRowable Movie where toMovieRowId = insertOrVerify

class MaybeMovieRowable a where
  toMaybeMovieRowId :: a -> DbMaybe MovieRowId
instance MovieRowable m => MaybeMovieRowable m where
  toMaybeMovieRowId m = lift $ toMovieRowId m
instance MaybeMovieRowable MovieId where
  toMaybeMovieRowId m = fst <$> valueAndRowId m
