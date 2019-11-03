{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}

module OMDB(
  Source(..),
  MovieScore(..),
  ApiCall,
  MovieScores(..),
  scoreMap,
  getMovieScores,
) where

import           Data.Map.Strict               (Map)
import           Data.Set                      (Set)
import           Text.InterpolatedString.Perl6 (qq)

import           Control.Monad.Trans.Maybe     (MaybeT(..))

import           API                           (ApiCall, ApiMaybe, Url(..), parseRemoteJson, readKey)
import           MovieDB.Types                 (ImdbId, pattern ImdbId, Movie)

import           OMDB.Internal                 (MovieScore(MovieScore, _score, _source), Source(..), parse)

import           Common.Foldables              (notNull)
import qualified Common.Maps                   as Maps
import           Common.Maybes                 (check)
import           Common.Operators
import qualified Common.Sets                   as Sets


getScore :: ImdbId -> ApiMaybe (Set MovieScore)
getScore (ImdbId imdbId) = fmap Sets.from $ MaybeT $ do
  key <- readKey "omdb"
  let query = Url [qq|http://www.omdbapi.com/?i=$imdbId&apikey=$key|]
  check notNull <$> parseRemoteJson query parse

data MovieScores = MovieScores
  { _movie  :: Movie
  , _scores :: Set MovieScore
  } deriving (Eq, Ord, Show)

scoreMap :: MovieScores -> Map Source Int
scoreMap = Maps.fromFoldable _source _score . _scores

getMovieScores :: Movie -> ImdbId -> ApiMaybe MovieScores
getMovieScores m id = MovieScores <$*> m <*> getScore id
