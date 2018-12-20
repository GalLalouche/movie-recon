{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}

module MovieDB.Database(
  init,
  clear,
  write,
  read,
  DbPath(..),
  DbCall( ..),
) where

import Prelude hiding (init, read)

import Common.IO (removeIfExists, createFileIfNotExists)
import Common.Operators
import Common.Foldables (headOpt)
import Common.Flippers (lotate2, lotate3)

import MovieDB.Types (Movie(..), MovieId(..))

import Data.Text (Text, unpack)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, Query, open, withConnection, execute, execute_, query)
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromRow


newtype DbPath = DbPath { path :: Text }
type DbCall a = ReaderT DbPath IO a

instance FromRow Movie where
  fromRow = Movie <$> (MovieId <$> field) <*> field
instance ToRow Movie where
  toRow (Movie (MovieId id) name) = toRow (id, name)

unpackedPath :: DbCall String
unpackedPath = unpack . path <$> ask

open' :: DbCall Connection
open' = (liftIO . open) =<< unpackedPath

withConnection' :: (Connection -> IO a) -> DbCall a
withConnection' f = (liftIO . flip withConnection f) =<< unpackedPath

executeWithConnection :: ToRow q => Query -> q -> DbCall ()
executeWithConnection = lotate3 execute ..> withConnection'

execute_WithConnection :: Query -> DbCall()
execute_WithConnection = lotate2 execute_ .> withConnection'

queryWithConnection :: (FromRow r, ToRow i) => Query -> i -> DbCall [r]
queryWithConnection = lotate3 query ..> withConnection'

init :: DbCall()
init = execute_WithConnection "CREATE TABLE IF NOT EXISTS movies (id TEXT PRIMARY KEY, name TEXT)"

clear :: DbCall()
clear = execute_WithConnection "DROP TABLE IF EXISTS movies" >> init

write :: Movie -> DbCall ()
write = executeWithConnection "INSERT INTO movies (id, name) values (?, ?)"

read :: MovieId -> DbCall (Maybe Movie)
read (MovieId id) = queryWithConnection "select * from movies where id = (?)" [id] <$$> headOpt
