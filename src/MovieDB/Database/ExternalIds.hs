{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.ExternalIds(
  init,
  clear,
  addExternalId,
  addNullExternalId,
  Nullable(..),
  externalId,
) where

import Data.Maybe                        (maybe)
import Data.Text                         (Text)
import Prelude                           hiding (id, init)

import Control.Monad                     ((>=>))
import Data.Functor                      (void)

import MovieDB.Database                  (DbCall)
import MovieDB.Database.Internal.TypesTH ()
import MovieDB.Database.Movies           (MovieRowId, toMovieRowId)
import MovieDB.Types                     (ExternalHost(IMDB), ExternalId, pattern ExternalId, Movie, mkImdbId, toExternalId)

import Database.Persist.Sql              (Filter, deleteWhere, entityVal, getBy, insert, runMigrationSilent)
import Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
ExternalIdRow sql=external_id
  movieId         MovieRowId
  host            ExternalHost
  externalId      Text Maybe
  UniqueMovieHost movieId host
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

clear :: DbCall()
clear = deleteWhere ([] :: [Filter ExternalIdRow])


addExternalId :: ExternalId -> DbCall ExternalIdRowId
addExternalId = toRow >=> insert where
  toRow :: ExternalId -> DbCall ExternalIdRow
  toRow (ExternalId m h i) = do
    mrid <- toMovieRowId m
    return $ ExternalIdRow mrid h (Just i)

addNullExternalId :: Movie -> ExternalHost -> DbCall ExternalIdRowId
addNullExternalId m h = do
  mrid <- toMovieRowId m
  let row = ExternalIdRow mrid h Nothing
  insert row

data Nullable a = NoRow | Null | NotNull a deriving (Show, Eq, Ord)

externalId :: Movie -> ExternalHost -> DbCall (Nullable ExternalId)
externalId m h = do
  row <- getBy =<< UniqueMovieHost <$> toMovieRowId m <*> return h
  let mmid = fmap (externalIdRowExternalId . entityVal) row :: Maybe (Maybe Text)
  return $ case mmid of
    Nothing  -> NoRow
    Just mid -> maybe Null externalIdCtor mid
  where
    idCtor = case h of IMDB -> mkImdbId
    externalIdCtor = NotNull . toExternalId m . idCtor