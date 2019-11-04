{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.ExternalId(
  init,
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
import MovieDB.Database.Internal.Common  (getKeyFor)
import MovieDB.Database.Internal.TypesTH ()
import MovieDB.Database.Movie            (MovieRowId)
import MovieDB.Types                     (ExternalHost(IMDB), ExternalId(_id), pattern ExternalId, ImdbId, IsExternalId, Movie, mkImdbId, toExternalId)

import Database.Persist.Sql              (Entity(..), entityVal, getBy, insert, runMigrationSilent)
import Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Common.Operators                  ((<*$>), (>$>))


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
ExternalIdRow sql=external_id
  movieId         MovieRowId
  host            ExternalHost
  externalId      Text Maybe
  UniqueMovieHost movieId host
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables


addExternalId :: ExternalId -> DbCall ExternalIdRowId
addExternalId = toRow >=> insert where
  toRow :: ExternalId -> DbCall ExternalIdRow
  toRow (ExternalId m h i) = ExternalIdRow <$> getKeyFor m <*$> h <*$> Just i

addNullExternalId :: Movie -> ExternalHost -> DbCall ExternalIdRowId
addNullExternalId m h = insert =<< ExternalIdRow <$> getKeyFor m <*$> h <*$> Nothing

addNullableExternalId :: IsExternalId eid => Movie -> ExternalHost -> Maybe eid -> DbCall ExternalIdRowId
addNullableExternalId movie host Nothing = addNullExternalId movie host
addNullableExternalId movie _ (Just id)  = addExternalId $ toExternalId movie id

externalId :: ExternalHost -> Movie -> DbCall (Nullable ExternalId)
externalId h m = do
  row <- getBy =<< UniqueMovieHost <$> getKeyFor m <*$> h
  return $ externalIdCtor <$> fromMaybeMaybe (fmap (externalIdRowExternalId . entityVal) row) where
    externalIdCtor = toExternalId m . idCtor
    idCtor = case h of IMDB -> mkImdbId

imdbId :: Movie -> DbCall (Nullable ImdbId)
imdbId = externalId IMDB >$> fmap (mkImdbId . _id)
