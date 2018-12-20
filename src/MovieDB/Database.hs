{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module MovieDB.Database(
  init,
  clear,
  write,
  read,
  DbPath(..),
  DbCall( ..),
) where

import Prelude hiding (init, read)

import Common.Operators

import MovieDB.Types (Movie(..), MovieId(..))

import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad (void)

import Database.Persist.Sql (deleteWhere, entityVal, getBy, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
MovieRow
  movieId         Text
  UniqueMovieId   movieId
  name            Text
|]

fromMovie :: Movie -> MovieRow
fromMovie (Movie (MovieId id) name) = MovieRow id name
toMovie :: MovieRow -> Movie
toMovie (MovieRow id name) = Movie (MovieId id) name

newtype DbPath = DbPath { path :: Text }
type DbCall a = ReaderT DbPath IO a

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter MovieRow])

write :: Movie -> DbCall ()
write (Movie (MovieId id) name) = void <$> withMigration $ insert $ MovieRow id name

read :: MovieId -> DbCall (Maybe Movie)
read (MovieId id) = withMigration $ do
  result <- getBy $ UniqueMovieId id
  return $ toMovie . entityVal <$> result
