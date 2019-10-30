{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module MovieDB.Database.Participation(
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

import           Prelude                           hiding (elem, init)

import           Data.Vector                       (Vector, elem)
import qualified Data.Vector                       as Vector (fromList)

import           Control.Monad.Trans.Maybe         (MaybeT(..), runMaybeT)
import           Data.Functor                      (void)

import           MovieDB.Database                  (DbCall, DbMaybe)
import           MovieDB.Database.Internal.Common  (getValueByRowId, insertOrVerify)
import           MovieDB.Database.Internal.TypesTH ()
import           MovieDB.Database.Movie           (MaybeMovieRowable, MovieRowId, MovieRowable, toMaybeMovieRowId, toMovieRowId)
import           MovieDB.Database.Person          (PersonRowId, PersonRowable, toPersonRowId)
import           MovieDB.Types                     (CastAndCrew, Movie, Participation(..), ParticipationType, Person, toCastAndCrew)

import           Database.Persist.Sql              (Filter, deleteWhere, entityKey, entityVal, getBy, insert, runMigrationSilent, selectList, (==.))
import           Database.Persist.TH               (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import           Common.Foldables                  (mapHeadOrElse)
import qualified Common.MaybeTs                    as MaybeTs (fromFoldable)
import           Common.Operators


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
ParticipationRow sql=participation
  personId  PersonRowId
  movieId   MovieRowId
  type      ParticipationType
  ParticipationUniqueness personId movieId type
|]

init :: DbCall ()
init = void $ runMigrationSilent migrateTables

clear :: DbCall ()
clear = deleteWhere ([] :: [Filter ParticipationRow])


getEntry :: PersonRowId -> MovieRowId -> ParticipationType -> DbMaybe ParticipationRowId
getEntry = fmap entityKey . MaybeT . getBy ..: ParticipationUniqueness

addEntry :: PersonRowId -> MovieRowId -> ParticipationType -> DbCall ParticipationRowId
addEntry p m pt = do
  existingId <- runMaybeT $ getEntry p m pt
  mapHeadOrElse return (insert $ ParticipationRow p m pt) existingId

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
  result <- Vector.fromList . map entityVal <$> selectList [column ==. rowId] []
  traverse toParticipation result

getParticipationsForMovie :: MovieRowable m => m -> DbCall (Vector Participation)
getParticipationsForMovie = participationsAux ParticipationRowMovieId toMovieRowId

getParticipationsForPerson :: PersonRowable p => p -> DbCall (Vector Participation)
getParticipationsForPerson = participationsAux ParticipationRowPersonId toPersonRowId

-- "Nothing" is returned if there are no participation entries for the movie.
castAndCrew :: MaybeMovieRowable m => m -> DbMaybe CastAndCrew
castAndCrew m = do
  mid <- toMaybeMovieRowId m
  toCastAndCrew <$> MaybeTs.fromFoldable (getParticipationsForMovie mid)

data EntryResult = Participated | DidNotParticipate | Unknown
  deriving (Show, Eq, Ord)
-- Returns Unknown the movie has no participations at all.
hasParticipated :: Person -> Movie -> DbCall EntryResult
hasParticipated p = getParticipationsForMovie >$> aux . fmap person where
  aux crew | null crew = Unknown | p `elem` crew = Participated | otherwise = DidNotParticipate
