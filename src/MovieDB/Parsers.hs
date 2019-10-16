{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module MovieDB.Parsers(
  parseMovieCredits,
  parsePersonCredits,
) where

import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)

import           Data.Foldable             (toList)
import           Data.Maybe                (mapMaybe)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text, pack)
import           Data.Time                 (Day)

import           Data.Aeson                (Object, Value)
import           Data.Aeson.Types          (Parser)

import           Common.JsonObjectParser   (ObjectParser, int, strMaybe, str, strReadMaybe, withObjects)
import           Common.Transes            ((>>=^), (>>=&))
import           Common.MonadPluses        (catMaybes)
import           Common.Operators

import           MovieDB.Types             (Movie (..), MovieId (..), ParticipationType (..), Person (..), PersonId (..))


getId :: (Text -> a) -> ObjectParser a
getId ctr = int "id" <$$> (show .> pack .> ctr)

parseCastAndCrew :: ObjectParser a -> ObjectParser [(a, ParticipationType)]
parseCastAndCrew parser = do
    parsedCast <- withObjects "cast" parseCast
    parsedCrew <- withObjects "crew" parseCrew <$$> catMaybes
    return . toList $ parsedCast <> parsedCrew where
      withRole role = (, role) <$> parser
      parseCast = withRole Actor
      -- Returns Nothing on unsupported role, e.g., Producer, or if the there is no "job" field, e.g., for future movies.
      parseCrew = runMaybeT $ MaybeT (strMaybe "job") >>=& toRole >>=^ withRole where
        toRole "Director"   = Just Director
        toRole "Screenplay" = Just Writer
        toRole _            = Nothing

parseMovieCredits :: ObjectParser [(Person, ParticipationType)]
parseMovieCredits = parseCastAndCrew parsePerson where
  parsePerson = Person <$> getId PersonId <*> str "name"

-- release_date is optional for unreleased movies
data MaybeMovie = MaybeMovie MovieId Text (Maybe Day)
parsePersonCredits :: ObjectParser [(Movie, ParticipationType)]
parsePersonCredits = mapMaybe liftMaybe <$> parseCastAndCrew parseMovie where
  parseMovie = MaybeMovie <$> getId MovieId <*> str "title" <*> strReadMaybe "release_date"
  liftMaybe :: (MaybeMovie, ParticipationType) -> Maybe (Movie, ParticipationType)
  liftMaybe (MaybeMovie id name d, pt) = fmap (\d -> (Movie id name d, pt)) d
