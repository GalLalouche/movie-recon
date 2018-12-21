{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module MovieDB.Database.Directors(
  init,
  clear,
  write,
  read,
  DbPath(..),
  DbCall( ..),
) where

import Prelude hiding (init, read)

import Common.Operators

import MovieDB.Database.Common (DbPath(..), DbCall)
import MovieDB.Types (Director(..), DirectorId(..), PersonId(..))

import Data.Text (Text)
import Control.Monad.Trans.Reader (ask)
import Control.Monad (void)

import Database.Persist.Sql (deleteWhere, entityVal, getBy, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
DirectorRow
  directorId         Text
  UniqueDirectorId   directorId
  name               Text
|]

--TODO lenses
deepId :: Director -> Text
deepId (Director (DirectorId (PersonId id)) _) = id

fromDirector :: Director -> DirectorRow
fromDirector (Director (DirectorId (PersonId id)) name) = DirectorRow id name
toDirector :: DirectorRow -> Director
toDirector (DirectorRow id name) = Director (DirectorId (PersonId id)) name

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter DirectorRow])

write :: Director -> DbCall ()
write = void <$> (withMigration . insert . fromDirector)

read :: DirectorId -> DbCall (Maybe Director)
read (DirectorId (PersonId id)) = withMigration $ do
  result <- getBy $ UniqueDirectorId id
  return $ toDirector . entityVal <$> result
