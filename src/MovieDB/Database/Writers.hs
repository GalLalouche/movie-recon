{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Writers(
  init,
  clear,
  insertOrVerify,
  WriterRowId,
) where

import Prelude hiding (init, id)

import Common.Operators

import MovieDB.Types (Writer(..), WriterId(..), PersonId(..))
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
WriterRow
  writerId      Text
  UniqueWriterId   writerId
  name            Text
|]


makeLensesWith classUnderscoreNoPrefixFields ''PersonId
makeLensesWith classUnderscoreNoPrefixFields ''WriterId
makeLensesWith classUnderscoreNoPrefixFields ''Writer

rowIso :: Iso' Writer WriterRow
rowIso = iso fromWriter toWriter where
  fromWriter :: Writer -> WriterRow
  fromWriter writer = WriterRow (writer ^. id ^. id ^. id) (writer ^. name)
  toWriter :: WriterRow -> Writer
  toWriter (WriterRow id name) = Writer (WriterId (PersonId id)) name

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter WriterRow])

invertIso :: WriterRow -> Writer
invertIso = view (from rowIso)

instance ExtractableId Writer WriterId where
  extractId = view id
instance ReadOnlyDatabase Writer WriterId WriterRowId where
  -- TODO handle code duplication
  valueAndRowId writerId = MaybeT $ withMigration $ do
    result <- getBy $ UniqueWriterId $ writerId ^. id ^. id
    return $  result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId writerRowId = withMigration $ (invertIso . fromJust) <$> get writerRowId
instance ReadWriteDatabase Writer WriterId WriterRowId where
  forceInsert = withMigration . insert . view rowIso

