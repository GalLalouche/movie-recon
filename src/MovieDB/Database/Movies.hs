{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, UndecidableInstances                               #-}


module MovieDB.Database.Movies(
  init,
  clear,
  getValue,
  insertOrVerify,
  MovieRowId,
  MovieRowable,
  toMovieRowId,
  MaybeMovieRowable,
  toMaybeMovieRowId,
) where

import           Prelude                    hiding (id, init)
import qualified Prelude                    (id, init)

import           Common.Operators

import           MovieDB.Database.Common    (DbCall(..), DbMaybe(..), DbPath(..), ExtractableId(..),
                                             ReadOnlyDatabase(..), ReadWriteDatabase(..), getRowId, getValue,
                                             insertOrVerify)
import           MovieDB.Types              (Movie(..), MovieId(..), PersonId(..))

import           Common.MaybeTUtils         (just)
import           Control.Arrow              ((&&&))
import           Control.Lens               (Iso', classUnderscoreNoPrefixFields, from, iso, makeLensesWith,
                                             view, (^.))
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Maybe  (MaybeT(..))
import           Control.Monad.Trans.Reader (ask)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Data.Time                  (Day)

import           Database.Persist.Sql       (Filter, deleteWhere, entityKey, entityVal, get, getBy, insert)
import           Database.Persist.Sqlite    (runMigrationSilent, runSqlite)
import           Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MovieRow
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
  toMovie (MovieRow id name date) = Movie (MovieId id) name date

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
    return $ result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId movieRowId = withMigration $ (invertIso . fromJust) <$> get movieRowId
instance ReadWriteDatabase Movie MovieId MovieRowId where
  forceInsert = withMigration . insert . view rowIso

class MovieRowable a where
  toMovieRowId :: a -> DbCall MovieRowId

instance MovieRowable MovieRowId where toMovieRowId = return
instance MovieRowable Movie where toMovieRowId = insertOrVerify

class MaybeMovieRowable a where
  toMaybeMovieRowId :: a -> DbMaybe MovieRowId
instance MovieRowable m => MaybeMovieRowable m where
  toMaybeMovieRowId m = just $ toMovieRowId m
instance MaybeMovieRowable MovieId where
  toMaybeMovieRowId m = fst <$> valueAndRowId m

