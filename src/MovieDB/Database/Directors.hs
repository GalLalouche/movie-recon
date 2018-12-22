{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Directors(
  init,
  clear,
  write,
  read,
) where

import Prelude hiding (init, read, id)

import Common.Operators

import MovieDB.Database.Common (DbPath(..), DbCall)
import MovieDB.Types (Director(..), DirectorId(..), PersonId(..))

import Data.Text (Text)
import Control.Monad.Trans.Reader (ask)
import Control.Monad (void)
import Control.Lens (makeLensesWith, classUnderscoreNoPrefixFields, Iso', iso, from, view, (^.))

import Database.Persist.Sql (deleteWhere, entityVal, getBy, insert, Filter)
import Database.Persist.Sqlite (runSqlite, runMigrationSilent)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
DirectorRow
  directorId         Text
  UniqueDirectorId   directorId
  name               Text
|]

makeLensesWith classUnderscoreNoPrefixFields ''PersonId
makeLensesWith classUnderscoreNoPrefixFields ''DirectorId
makeLensesWith classUnderscoreNoPrefixFields ''Director

rowIso :: Iso' Director DirectorRow
rowIso = iso fromDirector toDirector where
  fromDirector :: Director -> DirectorRow
  fromDirector director = DirectorRow (director ^. id ^. id ^. id) (director ^. name)
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
write = void <$> (withMigration . insert . view rowIso)

read :: DirectorId -> DbCall (Maybe Director)
read directorId = withMigration $ do
  result <- getBy $ UniqueDirectorId $ directorId ^. id ^. id
  return $ view (from rowIso) . entityVal <$> result
