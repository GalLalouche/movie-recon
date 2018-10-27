{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MovieDB.ParserTest where

import Prelude hiding (readFile)

import MovieDB.Parsers
import MovieDB.Types

import qualified Data.Aeson as J
import Data.ByteString.Lazy (readFile)

import Common.Unsafe

import Test.Tasty
import Test.Tasty.HUnit


test_MovieDBInternal = testGroup "parseJson" [
    testCase "parseSearchPerson" $ do
      json <- readFile "test/resources/foo.json"
      let res = parseSearchPerson $ right $ J.eitherDecode json :: PersonId
      res @=? PersonId 36592
  ]

