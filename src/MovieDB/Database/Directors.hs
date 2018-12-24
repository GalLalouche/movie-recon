{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Directors(
  init,
  clear,
  insertOrVerify,
  DirectorRowId,
) where

import Prelude hiding (init, id)

import Common.Operators

import MovieDB.Types (Director(..), DirectorId(..), PersonId(..))
import MovieDB.Database.Common (DbPath(..), DbCall(..), ExtractableId(..), ReadOnlyDatabase(..), ReadWriteDatabase(..), insertOrVerify)

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
DirectorRow
  directorId      Text
  UniqueDirectorId   directorId
  name            Text
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

invertIso :: DirectorRow -> Director
invertIso = view (from rowIso)

instance ExtractableId Director DirectorId where
  extractId = view id
instance ReadOnlyDatabase Director DirectorId DirectorRowId where
  -- TODO handle code duplication
  valueAndRowId directorId = MaybeT $ withMigration $ do
    result <- getBy $ UniqueDirectorId $ directorId ^. id ^. id
    return $  result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId directorRowId = MaybeT $ withMigration $ do
    result <- get directorRowId
    return $ invertIso <$> result
instance ReadWriteDatabase Director DirectorId DirectorRowId where
  forceInsert = withMigration . insert . view rowIso

