{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, ScopedTypeVariables, LambdaCase #-}

module MovieDB.Database.Participations where

import MovieDB.Database.Common (DbCall(..), ExtractableId, extractId, ReadOnlyDatabase, getRowId, getValueByRowId, ReadWriteDatabase, insertOrVerify)
import MovieDB.Database.Movies (MovieRowId)
import MovieDB.Types (Movie)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Data.Maybe (isJust)

import Common.Operators

class ParticipationReadOnly valueRowId where
  -- A movie can have multiple crew of the same type, e.g., multiple directors, writers, and of course actors.
  crewRowIds :: MovieRowId -> DbCall [valueRowId]
  -- A writer or director can obviously have multiple movies.
  movieRowIds :: valueRowId -> DbCall [MovieRowId]

crew :: forall value valueRowId typeId .
    (ReadOnlyDatabase value typeId valueRowId, ParticipationReadOnly valueRowId) => Movie -> DbCall [value]
crew movie = runMaybeT (getRowId $ extractId movie) >>= \case
  Nothing -> return []
  Just movieRowId -> do
    cris <- crewRowIds movieRowId
    traverse getValueByRowId cris

movies :: forall value valueRowId typeId .
    (ExtractableId value typeId, ReadOnlyDatabase value typeId valueRowId, ParticipationReadOnly valueRowId) =>
    value -> DbCall [Movie]
movies value = runMaybeT (getRowId $ extractId value) >>= \case
  Nothing -> return []
  Just rowId -> do
    movieRowIds <- movieRowIds rowId
    traverse getValueByRowId movieRowIds

data EntryResult = Participated | DidNotParticipate | Unknown -- When the movie has no entry
hasParticipated :: forall value valueRowId typeId p .
    (Eq value, ReadOnlyDatabase value typeId valueRowId, ParticipationReadOnly valueRowId) =>
    value -> Movie -> DbCall EntryResult
hasParticipated a movie = fromList <$> crew movie where
  fromList [] = Unknown
  fromList cast = if a `elem` cast then Participated else DidNotParticipate

class ParticipationDatabase valueRowId participationRowId | valueRowId -> participationRowId where
  addEntry :: MovieRowId -> valueRowId -> DbCall participationRowId

addValueEntry :: forall value valueRowId typeId participationRowId .
    (Show value, Eq value
    , ExtractableId value typeId
    , ReadWriteDatabase value typeId valueRowId
    , ParticipationDatabase valueRowId participationRowId) => Movie -> value -> DbCall participationRowId
addValueEntry movie v = do
  movieRowId <- insertOrVerify movie
  vRowId <- insertOrVerify v :: DbCall valueRowId
  addEntry movieRowId vRowId
