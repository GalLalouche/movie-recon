{-# LANGUAGE QuasiQuotes #-}

module OMDB(
  Source(..),
  MovieScore(..),
  ApiCall,
  MovieScores(..),
  scoreMap,
  getMovieScores,
) where

import           Data.Map.Strict           (Map)
import           Data.Set                  (Set)
import           Data.String.Interpolate   (i)

import           Control.Monad.Trans.Maybe (MaybeT(..))

import           APIs                      (ApiCall, ApiMaybe, Url(..), parseRemoteJson, readKey)
import           MovieDB.Types             (ImdbId(..), Movie)

import           OMDB.Internal             (MovieScore(MovieScore, _score, _source), Source(..), parse)

import           Common.Foldables          (notNull)
import qualified Common.Maps               as Maps
import qualified Common.Sets               as Sets
import           Common.Maybes             (check)


getScore :: ImdbId -> ApiMaybe (Set MovieScore)
getScore (ImdbId imdbId) = fmap Sets.from $ MaybeT $ do
  key <- readKey "omdb"
  let query = Url [i|http://www.omdbapi.com/?i=#{imdbId}&apikey=#{key}|]
  check notNull <$> parseRemoteJson query parse

data MovieScores = MovieScores
  { _movie  :: Movie
  , _scores :: Set MovieScore
  } deriving (Eq, Ord, Show)

scoreMap :: MovieScores -> Map Source Int
scoreMap = Maps.fromFoldable _source _score . _scores

getMovieScores :: Movie -> ImdbId -> ApiMaybe MovieScores
getMovieScores m id = do
  score <- getScore id
  return $ MovieScores m score
