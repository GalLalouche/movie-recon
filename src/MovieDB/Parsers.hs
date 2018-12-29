{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module MovieDB.Parsers(
  parseMovieCredits,
  parsePersonCredits,
) where

import           MovieDB.Types      (Movie(..), MovieId(..), ParticipationType(..), Person(..), PersonId(..))

import           Data.Foldable      (toList)
import           Data.Semigroup     ((<>))
import           Data.Text          (pack, unpack)

import           Data.Aeson         (Object, Value)
import           Data.Aeson.Types   (Parser)

import           Common.JsonUtils   (asObject, int, objects, str)
import           Common.MonadPluses
import           Common.Operators


getId ctr = int "id" >$> (show .> pack .> ctr)

-- TODO MaybeT
parseCrew withRole o = parseCrewRole o >>= \case
    Nothing -> return Nothing
    Just r -> Just <$> withRole r o
  where
    -- Returns Nothing on unsupported role, e.g., producer
    parseCrewRole :: Object -> Parser (Maybe ParticipationType)
    parseCrewRole o = do
      job <- o |> str "job"
      return $ case job of
        "Director"   -> Just Director
        "Screenplay" -> Just Writer
        _            -> Nothing

parseCastAndCrew parser o = do
    object <- o |> asObject
    cast <- object |> objects "cast"
    crew <- object |> objects "crew"
    parsedCast <- traverse parseCast cast
    parsedCrew <- catMaybes <$> traverse (parseCrew withRole) crew
    return $ toList $ parsedCast <> parsedCrew where
      withRole role o = flip (,) role <$> parser o
      parseCast = withRole Actor

parseMovieCredits :: Value -> Parser [(Person, ParticipationType)]
parseMovieCredits = parseCastAndCrew parsePerson where
  parsePerson o = Person <$> getId PersonId o <*> str "name" o

parsePersonCredits :: Value -> Parser [(Movie, ParticipationType)]
parsePersonCredits = parseCastAndCrew parseMovie where
  parseMovie o = Movie <$> getId MovieId o <*> str "title" o <*> getDate o
  getDate = str "release_date" >$> (read . unpack)
