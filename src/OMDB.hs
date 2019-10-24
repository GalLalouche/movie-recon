{-# LANGUAGE QuasiQuotes #-}

module OMDB(
  Source(..),
  MovieScore(..),
  ApiCall,
  MovieScores(..),
  getMovieScores,
) where

import Data.String.Interpolate   (i)
import Data.Vector               (Vector)

import Control.Monad.Trans.Maybe (MaybeT(..))

import MovieDB.Types             (ImdbId(..), Movie)
import APIs                      (ApiCall, ApiMaybe, Url(..), parseRemoteJson, readKey)

import OMDB.Internal             (MovieScore(..), Source(..), parse)

import Common.Foldables          (notNull)
import Common.Maybes             (check)


getScore :: ImdbId -> ApiMaybe (Vector MovieScore)
getScore (ImdbId imdbId) = MaybeT $ do
  key <- readKey "omdb"
  let query = Url [i|http://www.omdbapi.com/?i=#{imdbId}&apikey=#{key}|]
  check notNull <$> parseRemoteJson query parse

data MovieScores = MovieScores
  { _movie  :: Movie
  , _scores :: Vector MovieScore
  } deriving (Eq, Ord, Show)

getMovieScores :: Movie -> ImdbId -> ApiMaybe MovieScores
getMovieScores m id = do
  score <- getScore id
  return $ MovieScores m score
