{-# LANGUAGE DuplicateRecordFields #-}

module MovieDB.Types where

import Data.Text (Text)
import Data.Time (Day)
import Common.Maps (multiMapBy, monoidLookup)
import Data.List.NonEmpty (NonEmpty((:|)))

newtype PersonId = PersonId
  { _id :: Text
  } deriving (Show, Eq, Ord)

data Person = Person
  { _id :: PersonId
  , _name :: Text
  } deriving (Show, Eq, Ord)

class HasDeepId a where
  deepId :: a -> Text

newtype MovieId = MovieId
  { _id :: Text
  } deriving (Show, Eq, Ord)

data Movie = Movie
  { _id :: MovieId
  , _name :: Text
  , _date :: Day
  } deriving (Show, Eq, Ord)

instance HasDeepId Movie where
  deepId m = _id (_id (m :: Movie) :: MovieId)

instance HasDeepId Person where
  deepId p = _id (_id (p :: Person) :: PersonId)

data CastAndCrew = CastAndCrew
  { movie :: Movie
  , directors :: [Person]
  , writers :: [Person]
  , actors :: [Person]
  } deriving (Show, Eq, Ord)

data ParticipationType = Director | Writer | Actor deriving (Show, Read, Eq, Ord)

data Participation = Participation
  { person :: Person
  , movie :: Movie
  , participationType :: ParticipationType
  } deriving (Show, Eq, Ord)

toCastAndCrew :: NonEmpty Participation -> CastAndCrew
toCastAndCrew ps@(p :| _) = let
    map = multiMapBy participationType ps
    m = movie (p :: Participation)
    getAll pt = person <$> monoidLookup pt map
    directors = getAll Director
    writers = getAll Writer
    actors = getAll Actor
  in CastAndCrew { movie = m, directors = directors, writers = writers, actors = actors }

