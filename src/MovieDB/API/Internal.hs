{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module MovieDB.API.Internal(
  parseMovieCredits,
  parsePersonCredits,
  parseId,
  parsePersonName,
  parseImdbId,
) where

import           Control.Monad             (mfilter)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import           Data.Semigroup            ((<>))
import           Data.Text                 (Text, pack, splitOn, unpack)
import qualified Data.Text                 as Text (null)
import           Data.Time                 (Day)
import           Data.Vector               (Vector)

import           Common.JsonUtils          (ObjectParser, int, str, strMaybe, withObjects)
import           Common.Maybes             (mapIfOrNothing)
import           Common.MonadPluses        (catMaybes, mapMaybe)
import           Common.Operators
import           Common.Transes            ((>>=&), (>>=^))

import           API                      (Url(..))
import           MovieDB.Types             (ImdbId, Movie(..), MovieId, ParticipationType(..), Person(..), PersonId, mkImdbId, mkMovieId, mkPersonId)


getId :: (Text -> a) -> ObjectParser a
getId ctr = int "id" <$$> (show .> pack .> ctr)

notNull :: Text -> Bool
notNull = not . Text.null

parseCastAndCrew :: ObjectParser a -> ObjectParser (Vector (a, ParticipationType))
parseCastAndCrew parser = do
    parsedCast <- withObjects "cast" parseCast
    parsedCrew <- withObjects "crew" parseCrew <$$> catMaybes
    return $ parsedCast <> parsedCrew where
      withRole role = (, role) <$> parser
      parseCast = withRole Actor
      -- Returns Nothing on unsupported role, e.g., Producer, or if the there is no "job" field, e.g., for future movies.
      parseCrew = runMaybeT $ MaybeT (strMaybe "job") >>=& toRole >>=^ withRole where
        toRole "Director"   = Just Director
        toRole "Screenplay" = Just Writer
        toRole _            = Nothing

parseMovieCredits :: ObjectParser (Vector (Person, ParticipationType))
parseMovieCredits = parseCastAndCrew parsePerson where
  parsePerson = Person <$> getId mkPersonId <*> str "name"

-- release_date is optional for unreleased movies
data MaybeMovie = MaybeMovie MovieId Text (Maybe Day)
parsePersonCredits :: ObjectParser (Vector (Movie, ParticipationType))
parsePersonCredits = mapMaybe liftMaybe <$> parseCastAndCrew parseMovie where
  parseMovie = MaybeMovie <$> getId mkMovieId <*> str "title" <*> getDay
  getDay :: ObjectParser (Maybe Day)
  getDay = runMaybeT $ (MaybeT $ strMaybe "release_date") >>=& mapIfOrNothing notNull (read . unpack)
  liftMaybe :: (MaybeMovie, ParticipationType) -> Maybe (Movie, ParticipationType)
  liftMaybe (MaybeMovie id name d, pt) = fmap (\d -> (Movie id name d, pt)) d

parseId :: Url -> PersonId
parseId (Url url) = mkPersonId $ head $ splitOn "-" $ splitOn "/person/" url !! 1

parsePersonName :: ObjectParser Text
parsePersonName = str "name"

parseImdbId :: ObjectParser (Maybe ImdbId)
parseImdbId = fmap mkImdbId . mfilter notNull <$> strMaybe "imdb_id"
