{-# LANGUAGE TypeFamilies, OverloadedStrings, GADTs, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}


module MovieDB.Database.Actors(
  init,
  clear,
  insertOrVerify,
  ActorRowId,
) where

import Prelude hiding (init, id)

import Common.Operators

import MovieDB.Types (Actor(..), ActorId(..), PersonId(..))
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
ActorRow
  actorId      Text
  UniqueActorId   actorId
  name            Text
|]


makeLensesWith classUnderscoreNoPrefixFields ''PersonId
makeLensesWith classUnderscoreNoPrefixFields ''ActorId
makeLensesWith classUnderscoreNoPrefixFields ''Actor

rowIso :: Iso' Actor ActorRow
rowIso = iso fromActor toActor where
  fromActor :: Actor -> ActorRow
  fromActor actor = ActorRow (actor ^. id ^. id ^. id) (actor ^. name)
  toActor :: ActorRow -> Actor
  toActor (ActorRow id name) = Actor (ActorId (PersonId id)) name

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter ActorRow])

invertIso :: ActorRow -> Actor
invertIso = view (from rowIso)

instance ExtractableId Actor ActorId where
  extractId = view id
instance ReadOnlyDatabase Actor ActorId ActorRowId where
  -- TODO handle code duplication
  valueAndRowId actorId = MaybeT $ withMigration $ do
    result <- getBy $ UniqueActorId $ actorId ^. id ^. id
    return $  result <$$> (entityKey &&& invertIso . entityVal)
  getValueByRowId actorRowId = MaybeT $ withMigration $ do
    result <- get actorRowId
    return $ invertIso <$> result
instance ReadWriteDatabase Actor ActorId ActorRowId where
  forceInsert = withMigration . insert . view rowIso
