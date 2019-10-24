{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

module MovieDB.Types(
  PersonId(..),
  Person(..),
  HasDeepId(..),
  MovieId(..),
  Movie(..),
  FilterReason(..),
  FilteredMovie(..),
  CastAndCrew(..),
  ParticipationType(..),
  Participation(..),
  mkPersonId,
  mkMovieId,
  toCastAndCrew,
  ImdbId(..),
  mkImdbId,
) where

import           Data.List.NonEmpty      (NonEmpty((:|)))
import           Data.Maybe              (isJust)
import           Data.String.Interpolate (i)
import           Data.Text               (Text, unpack)
import qualified Data.Text               as Text
import           Data.Time               (Day)
import           Text.Regex              (matchRegex, mkRegex)

import           Common.Maps             (monoidLookup, multiMapBy)
import           Common.Maybes           (check, orError)


newtype PersonId = PersonId
  { _id :: Text
  } deriving (Show, Eq, Ord)

data Person = Person
  { _id   :: PersonId
  , _name :: Text
  } deriving (Show, Eq, Ord)

class HasDeepId a where
  deepId :: a -> Text

newtype MovieId = MovieId
  { _id :: Text
  } deriving (Show, Eq, Ord)

data Movie = Movie
  { _id   :: MovieId
  , _name :: Text
  , _date :: Day
  } deriving (Show, Eq, Ord)

data FilterReason = Ignored | Seen deriving (Show, Read, Eq, Ord)
data FilteredMovie = FilteredMovie
  { _movie  :: Movie
  , _reason :: FilterReason
  } deriving (Show, Eq, Ord)

instance HasDeepId Movie where
  deepId m = _id (_id (m :: Movie) :: MovieId)

instance HasDeepId Person where
  deepId p = _id (_id (p :: Person) :: PersonId)

data CastAndCrew = CastAndCrew
  { movie     :: Movie
  , directors :: [Person]
  , writers   :: [Person]
  , actors    :: [Person]
  } deriving (Show, Eq, Ord)

data ParticipationType = Director | Writer | Actor deriving (Show, Read, Eq, Ord)

data Participation = Participation
  { person            :: Person
  , movie             :: Movie
  , participationType :: ParticipationType
  } deriving (Show, Eq, Ord)


checkValidId :: Text -> (Text -> a) -> Text -> a
checkValidId name ctor s = if isValidId s then ctor s else error [i|<#{s}> is not a valid #{name} ID|] where
  isValidId = isJust . matchRegex digitsOnly . unpack
  digitsOnly = mkRegex "^[0-9]+$"

mkPersonId :: Text -> PersonId
mkPersonId = checkValidId "Person" PersonId

mkMovieId :: Text -> MovieId
mkMovieId = checkValidId "Movie" MovieId

toCastAndCrew :: NonEmpty Participation -> CastAndCrew
toCastAndCrew ps@(p :| _) = let
    map = multiMapBy participationType ps
    m = movie (p :: Participation)
    getAll pt = person <$> monoidLookup pt map
    directors = getAll Director
    writers = getAll Writer
    actors = getAll Actor
  in CastAndCrew { movie = m, directors = directors, writers = writers, actors = actors }

newtype ImdbId = ImdbId Text deriving (Eq, Ord, Show)
mkImdbId :: Text -> ImdbId
mkImdbId t = ImdbId $ orError [i|<#{t}> is not a valid IMDB Movie ID|] $ check (("tt" ==) . Text.take 2) t
