{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Movies(
  init,
  clear,
  getValue,
  insertOrVerify,
  MovieRowId,
) where

import Prelude hiding (init, id)

import Common.Operators

import MovieDB.Types (Movie(..), MovieId(..), PersonId(..))
import MovieDB.Database.Common (DbPath(..), DbCall(..), ExtractableId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), insertOrVerify, getValue)

import Data.Text (Text)
import Control.Arrow ((&&&))
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Data.Maybe (fromJust)
import Control.Lens (makeLensesWith, classUnderscoreNoPrefixFields, Iso', iso, from, view, (^.))

import Database.Persist.Sql (deleteWhere, entityVal, entityKey, get, getBy, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MovieRow
  movieId      Text
  UniqueMovieId   movieId
  name            Text
|]


makeLensesWith classUnderscoreNoPrefixFields ''MovieId
makeLensesWith classUnderscoreNoPrefixFields ''Movie

rowIso :: Iso' Movie MovieRow
rowIso = iso fromMovie toMovie where
  fromMovie :: Movie -> MovieRow
  fromMovie movie = MovieRow (movie ^. id ^. id) (movie ^. name)
  toMovie :: MovieRow -> Movie
  toMovie (MovieRow id name) = Movie (MovieId id) name

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter MovieRow])

invertIso :: MovieRow -> Movie
invertIso = view (from rowIso)

instance ExtractableId Movie MovieId where
  extractId = view id
instance ReadOnlyDatabase Movie MovieId MovieRowId where
  -- TODO handle code duplication
  valueAndRowId movieId = MaybeT $ withMigration $ do
    result <- getBy $ UniqueMovieId $ movieId ^. id
    return $  result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId movieRowId = withMigration $ (invertIso . fromJust) <$> get movieRowId
instance ReadWriteDatabase Movie MovieId MovieRowId where
  forceInsert = withMigration . insert . view rowIso

