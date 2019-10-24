module OMDB.Internal.InternalTest where

import OMDB.Internal        (MovieScore(..), Source(IMDB, Metacritic, RottenTomatoes), parse)

import Common.JsonTestUtils (parseJson)
import Common.TestCommon    ((*?=))
import Test.Tasty
import Test.Tasty.HUnit


test_OMDB_parsing = [
    testCase "high scores" $ do
      result <- parseJson parse "OMDB/Internal/high_scores"
      result *?= [MovieScore IMDB 90, MovieScore RottenTomatoes 94, MovieScore Metacritic 84]
  , testCase "low scores" $ do
      result <- parseJson parse "OMDB/Internal/low_scores"
      result *?= [MovieScore IMDB 44, MovieScore RottenTomatoes 4, MovieScore Metacritic 1]
  , testCase "no scores" $ do
      result <- parseJson parse "OMDB/Internal/no_scores"
      result *?= []
  ]
