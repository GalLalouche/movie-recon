{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module MovieDB.API.InternalTest where

import MovieDB.API.Internal (ApiKey(..), MovieCredits(..), PersonCredits(..), apiPath)
import MovieDB.Types        (MovieId(..), PersonId(..))

import Test.Tasty
import Test.Tasty.HUnit

test_MovieDB_API_internal = testGroup "apiPath" [
    testCase "movieCredits" $ do
      let res = apiPath (MovieCredits $ MovieId "1234") (ApiKey "api_key")
      res @?= "http://api.themoviedb.org/3/movie/1234/credits?api_key=api_key&language=en-US"
  , testCase "personCredits" $ do
      let res = apiPath (PersonCredits $ PersonId "1234") (ApiKey "api_key")
      res @?= "http://api.themoviedb.org/3/person/1234/movie_credits?api_key=api_key&language=en-US"
  ]
