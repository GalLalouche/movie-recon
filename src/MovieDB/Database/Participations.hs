{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.Participations(
  init,
  clear,
  addEntry,
  addValueEntry,
  hasParticipated,
  getParticipationsForMovie,
  getParticipationsForPerson,
  EntryResult(..),
  castAndCrew,
) where

import Prelude                    hiding (init)

import Control.Monad.Trans.Maybe  (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Reader (ask)

import Common.Foldables           (mapHeadOrElse)

import Common.MaybeTUtils         (fromList)
import Common.Operators

import MovieDB.Database.Common    (DbCall, DbMaybe, getValueByRowId, insertOrVerify, path)
import MovieDB.Database.Movies    (MaybeMovieRowable, MovieRowId, MovieRowable, toMaybeMovieRowId, toMovieRowId)
import MovieDB.Database.Persons   (PersonRowId, PersonRowable, toPersonRowId)
import MovieDB.Database.TypesTH   ()
import MovieDB.Types              (CastAndCrew, Movie, Participation(..), ParticipationType, Person, toCastAndCrew)

import Database.Persist.Sql       (Filter, deleteWhere, entityKey, entityVal, getBy, insert, selectList, (==.))
import Database.Persist.Sqlite    (runMigrationSilent, runSqlite)
import Database.Persist.TH        (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
ParticipationRow
  personId  PersonRowId
  movieId   MovieRowId
  type      ParticipationType
  ParticipationUniqueness personId movieId type
|]

withMigration action = do
  dbPath <- path <$> ask
  runSqlite dbPath (runMigrationSilent migrateTables >> action)


init :: DbCall()
init = withMigration $ return ()

clear :: DbCall()
clear = withMigration $ deleteWhere ([] :: [Filter ParticipationRow])

getEntry :: PersonRowId -> MovieRowId -> ParticipationType -> DbMaybe ParticipationRowId
getEntry = fmap entityKey . MaybeT . withMigration . getBy ..: ParticipationUniqueness

addEntry :: PersonRowId -> MovieRowId -> ParticipationType -> DbCall ParticipationRowId
addEntry p m pt = do
  existingId <- runMaybeT $ getEntry p m pt
  mapHeadOrElse return (withMigration $ insert $ ParticipationRow p m pt) existingId

addValueEntry :: Participation -> DbCall ParticipationRowId
addValueEntry (Participation person movie pt) = do
  pri <- insertOrVerify person
  mri <- insertOrVerify movie
  addEntry pri mri pt

toParticipation :: ParticipationRow -> DbCall Participation
toParticipation (ParticipationRow pid mid pt) =
 Participation <$> getValueByRowId pid <*> getValueByRowId mid <*> return pt

participationsAux column rowIdExtractor value = do
  rowId <- rowIdExtractor value
  result <- withMigration $ map entityVal <$> selectList [column ==. rowId] []
  traverse toParticipation result

getParticipationsForMovie :: MovieRowable m => m -> DbCall [Participation]
getParticipationsForMovie = participationsAux ParticipationRowMovieId toMovieRowId

getParticipationsForPerson :: PersonRowable p => p -> DbCall [Participation]
getParticipationsForPerson = participationsAux ParticipationRowPersonId toPersonRowId

-- "Nothing" is returned if there are no participation entries for the movie.
castAndCrew :: MaybeMovieRowable m => m -> DbMaybe CastAndCrew
castAndCrew m = do
  mid <- toMaybeMovieRowId m
  toCastAndCrew <$> fromList (getParticipationsForMovie mid)

data EntryResult = Participated | DidNotParticipate | Unknown -- When the movie has no participations at all
hasParticipated :: Person -> Movie -> DbCall EntryResult
hasParticipated p movie = fromList . map person <$> getParticipationsForMovie movie where
  fromList []   = Unknown
  fromList crew = if p `elem` crew then Participated else DidNotParticipate
