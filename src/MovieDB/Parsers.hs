{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module MovieDB.Parsers(
  parseMovieCredits,
  parsePersonCredits,
  Url(..),
  parseId,
  parsePersonName,
) where

import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import           Data.Foldable             (toList)
import           Data.Maybe                (mapMaybe)
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text, pack, splitOn, unpack)
import qualified Data.Text                 as Text (null)
import           Data.Time                 (Day)

import           Common.JsonUtils          (ObjectParser, int, str, strMaybe, withObjects)
import           Common.Maybes             (check, mapIfOrNothing)
import           Common.MonadPluses        (catMaybes)
import           Common.Operators
import           Common.Transes            ((>>=&), (>>=^))

import           MovieDB.Types             (Movie(..), MovieId(..), ParticipationType(..), Person(..), PersonId(..), mkPersonId)


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
  parseMovie = MaybeMovie <$> getId MovieId <*> str "title" <*> getDay
  getDay :: ObjectParser (Maybe Day)
  getDay = str "release_date" <$$> mapIfOrNothing (not . Text.null) (read . unpack)
  liftMaybe :: (MaybeMovie, ParticipationType) -> Maybe (Movie, ParticipationType)
  liftMaybe (MaybeMovie id name d, pt) = fmap (\d -> (Movie id name d, pt)) d

newtype Url = Url Text
parseId :: Url -> PersonId
parseId (Url url) = mkPersonId $ head $ splitOn "-" $ splitOn "/person/" url !! 1

parsePersonName :: ObjectParser Text
parsePersonName = str "name"
