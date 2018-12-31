{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Parsers(
  parseMovieCredits,
  parsePersonCredits,
) where

import MovieDB.Types             (Movie(..), MovieId(..), ParticipationType(..), Person(..), PersonId(..))

import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Foldable             (toList)
import Data.Semigroup            ((<>))
import Data.Text                 (Text, pack, unpack)

import Data.Aeson                (Object, Value)
import Data.Aeson.Types          (Parser)

import Common.JsonObjectParser   (ObjectParser, int, str, strRead, withObjects)
import Common.MaybeTUtils        (just)
import Common.MonadPluses        (catMaybes)
import Common.Operators


getId :: (Text -> a) -> ObjectParser a
getId ctr = int "id" <$$> (show .> pack .> ctr)

parseCastAndCrew :: ObjectParser a -> ObjectParser [(a, ParticipationType)]
parseCastAndCrew parser = do
    parsedCast <- withObjects "cast" parseCast
    parsedCrew <- withObjects "crew" parseCrew <$$> catMaybes
    return . toList $ parsedCast <> parsedCrew where
      withRole role = flip (,) role <$> parser
      parseCast = withRole Actor
      -- Returns Nothing on unsupported role, e.g., Producer.
      parseCrew = runMaybeT $ MaybeT (toRole <$> str "job") >>= just . withRole where
        toRole "Director"   = Just Director
        toRole "Screenplay" = Just Writer
        toRole _            = Nothing

parseMovieCredits :: ObjectParser [(Person, ParticipationType)]
parseMovieCredits = parseCastAndCrew parsePerson where
  parsePerson = Person <$> getId PersonId <*> str "name"

parsePersonCredits :: ObjectParser [(Movie, ParticipationType)]
parsePersonCredits = parseCastAndCrew parseMovie where
  parseMovie = Movie <$> getId MovieId <*> str "title" <*> strRead "release_date"
