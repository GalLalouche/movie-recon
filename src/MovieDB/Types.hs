{-# LANGUAGE DuplicateRecordFields #-}

module MovieDB.Types where

import Prelude hiding (id)
import Control.Lens.TH
import Data.Text (Text)


newtype PersonId = PersonId { _id :: Text } deriving (Show, Eq, Ord)

class Person a where
  personId :: a -> PersonId
  personName :: a -> Text
  makePerson :: PersonId -> Text -> a

deepId :: Person p => p -> Text
deepId p = _id (personId p :: PersonId)

newtype DirectorId = DirectorId { _id :: PersonId } deriving (Show, Eq, Ord)
data Director = Director { _id :: DirectorId, _name :: Text } deriving (Show, Eq, Ord)

instance Person Director where
  personId d = let dId = _id (d :: Director) in _id (dId :: DirectorId)
  personName = _name
  makePerson = Director . DirectorId

newtype WriterId = WriterId { _id :: PersonId } deriving (Show, Eq, Ord)
data Writer = Writer { _id :: WriterId, _name :: Text } deriving (Show, Eq, Ord)

instance Person Writer where
  personId d = let dId = _id (d :: Writer) in _id (dId :: WriterId)
  personName = _name
  makePerson = Writer . WriterId

newtype ActorId = ActorId { _id :: PersonId } deriving (Show, Eq, Ord)
data Actor = Actor { _id :: ActorId, _name :: Text } deriving (Show, Eq, Ord)

instance Person Actor where
  personId d = let dId = _id (d :: Actor) in _id (dId :: ActorId)
  personName = _name
  makePerson = Actor . ActorId

newtype MovieId = MovieId { _id :: Text } deriving (Show, Eq, Ord)
data Movie = Movie { _id :: MovieId, _name :: Text} deriving (Show, Eq, Ord)

data CastAndCrew = CastAndCrew {
    _movie :: Movie
  , _director :: Director
  , _writer :: Writer
  , _cast :: [Actor]
} deriving (Show, Eq, Ord)

newtype CastAndCrewId = CastAndCrewId { _d :: MovieId }

data CastAndCrewIds = CastAndCrewIds {
    _movieId :: MovieId
  , _directorId :: DirectorId
  , _writerId :: WriterId
  , _cast :: [ActorId]
} deriving (Show, Eq, Ord)

castAndCrewIds :: CastAndCrew -> CastAndCrewIds
castAndCrewIds (CastAndCrew m d w cs) =
  CastAndCrewIds (_id (m :: Movie)) (_id (d :: Director)) (_id (w :: Writer)) (map (\a -> _id (a :: Actor)) cs)

newtype Query = Query { query :: Text }
