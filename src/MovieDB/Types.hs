{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}

module MovieDB.Types(
  PersonId,
  pattern PersonId,
  Person(..),
  HasDeepId(..),
  MovieId,
  pattern MovieId,
  Movie(..),
  isReleased,
  FilterReason(..),
  FilteredMovie(..),
  CastAndCrew(..),
  ParticipationType(..),
  Participation(..),
  mkPersonId,
  mkMovieId,
  toCastAndCrew,
  ExternalHost(..),
  ExternalId,
  pattern ExternalId,
  toExternalId,
  ImdbId,
  pattern ImdbId,
  mkImdbId,
) where

import           Data.List.NonEmpty            (NonEmpty((:|)))
import           Data.Maybe                    (isJust)
import           Data.Text                     (Text, unpack)
import qualified Data.Text                     as Text
import           Data.Time                     (Day)
import           Data.Vector                   (Vector)
import qualified Data.Vector                   as Vector
import           Text.InterpolatedString.Perl6 (qq)
import           Text.Regex                    (matchRegex, mkRegex)

import           Common.Assertions             (assertMsg)
import           Common.Maps                   (monoidLookup, multiMapBy)


newtype PersonId = RealPersonId
  { _id :: Text
  } deriving (Show, Eq, Ord)
pattern PersonId :: Text -> PersonId
pattern PersonId id <- RealPersonId id
{-# COMPLETE PersonId #-}
mkPersonId :: Text -> PersonId
mkPersonId = checkDigitOnlyId "Person" RealPersonId

data Person = Person
  { _id   :: PersonId
  , _name :: Text
  } deriving (Show, Eq, Ord)

class HasDeepId a where
  deepId :: a -> Text

newtype MovieId = RealMovieId
  { _id :: Text
  } deriving (Show, Eq, Ord)
pattern MovieId :: Text -> MovieId
pattern MovieId id <- RealMovieId id
{-# COMPLETE MovieId #-}
mkMovieId :: Text -> MovieId
mkMovieId = checkDigitOnlyId "Movie" RealMovieId

checkValidId :: (Text -> Bool) -> Text -> (Text -> a) -> Text -> a
checkValidId check name ctor s = assertMsg (check s) [qq|<$s> is not a valid $name ID|] (ctor s)

checkDigitOnlyId = checkValidId isValidId where
  isValidId = isJust . matchRegex digitsOnly . unpack
  digitsOnly = mkRegex "^[0-9]+$"

data Movie = Movie
  { _id   :: MovieId
  , _name :: Text
  , _date :: Day
  } deriving (Show, Eq, Ord)

isReleased :: Day -> Movie -> Bool
isReleased today (Movie _ _ date) = date <= today

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
  , directors :: Vector Person
  , writers   :: Vector Person
  , actors    :: Vector Person
  } deriving (Show, Eq, Ord)
toCastAndCrew :: NonEmpty Participation -> CastAndCrew
toCastAndCrew ps@(p :| _) = let
    map = multiMapBy participationType ps
    m = movie (p :: Participation)
    getAll pt = Vector.fromList $ person <$> monoidLookup pt map
    directors = getAll Director
    writers = getAll Writer
    actors = getAll Actor
  in CastAndCrew { movie = m, directors = directors, writers = writers, actors = actors }

data ParticipationType = Director | Writer | Actor deriving (Show, Read, Eq, Ord)

data Participation = Participation
  { person            :: Person
  , movie             :: Movie
  , participationType :: ParticipationType
  } deriving (Show, Eq, Ord)

-- The only way to create an External ID is via toExternalId function.
data ExternalHost = IMDB deriving (Eq, Ord, Show, Read)
data ExternalId = RealExternalId
  { _movie :: Movie
  , _host  :: ExternalHost
  , _id    :: Text
  } deriving (Show, Eq, Ord)
pattern ExternalId :: Movie -> ExternalHost -> Text -> ExternalId
pattern ExternalId m h id <- RealExternalId m h id
{-# COMPLETE ExternalId #-}

class IsExternalId a where
  toExternalId :: Movie -> a -> ExternalId

newtype ImdbId = RealImdbId Text deriving (Eq, Ord, Show)
pattern ImdbId :: Text -> ImdbId
pattern ImdbId id <- RealImdbId id
{-# COMPLETE ImdbId #-}
mkImdbId :: Text -> ImdbId
mkImdbId = checkValidId startsWithTT "IMDB Movie" RealImdbId where startsWithTT = ("tt" ==) . Text.take 2

instance IsExternalId ImdbId where
  toExternalId m (ImdbId id) = RealExternalId m IMDB id
