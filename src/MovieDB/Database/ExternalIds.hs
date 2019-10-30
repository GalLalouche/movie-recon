{-# LANGUAGE DeriveFunctor              #-}
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
  addNullableExternalId,
  externalId,
  imdbId,
) where

import Data.Text                         (Text)
import Prelude                           hiding (id, init)

import Control.Monad                     ((>=>))
import Data.Functor                      (void)

import MovieDB.Database                  (DbCall, Nullable, fromMaybeMaybe)
import MovieDB.Database.Internal.TypesTH ()
import MovieDB.Database.Movies           (MovieRowId, toMovieRowId)
import MovieDB.Types                     (ExternalHost(IMDB), ExternalId, pattern ExternalId, ImdbId, IsExternalId, Movie, mkImdbId, toExternalId)

import Database.Persist.Sql              (Entity(..), Filter, deleteWhere, entityVal, getBy, insert, runMigrationSilent)
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

addNullableExternalId :: IsExternalId eid => Movie -> ExternalHost -> Maybe eid -> DbCall ExternalIdRowId
addNullableExternalId movie host Nothing = addNullExternalId movie host
addNullableExternalId movie _ (Just id)  = addExternalId $ toExternalId movie id

externalId :: Movie -> ExternalHost -> DbCall (Nullable ExternalId)
externalId m h = do
  row <- getBy =<< UniqueMovieHost <$> toMovieRowId m <*> return h
  return $ externalIdCtor <$> fromMaybeMaybe (fmap (externalIdRowExternalId . entityVal) row) where
    externalIdCtor = toExternalId m . idCtor
    idCtor = case h of IMDB -> mkImdbId

imdbId :: Movie -> DbCall (Nullable ImdbId)
imdbId m = fmap (\(ExternalId _ _ id) -> mkImdbId id) <$> externalId m IMDB
