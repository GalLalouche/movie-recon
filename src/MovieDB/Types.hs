{-# LANGUAGE DuplicateRecordFields #-}

module MovieDB.Types where

import Data.Text (Text)
import Data.Time (Day)

newtype PersonId = PersonId
  { _id :: Text
  } deriving (Show, Eq, Ord)

data Person = Person
  { _id :: PersonId
  , _name :: Text
  } deriving (Show, Eq, Ord)

deepId :: Person -> Text
deepId p = let personId = _id (p :: Person) in _id (personId :: PersonId)

newtype MovieId = MovieId
  { _id :: Text
  } deriving (Show, Eq, Ord)

data Movie = Movie
  { _id :: MovieId
  , _name :: Text
  , _date :: Day
  } deriving (Show, Eq, Ord)

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
  }

newtype Query = Query
  { query :: Text
  }
