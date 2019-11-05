module Common.MapsTest where

import qualified Common.Maps      as Maps
import qualified Data.Map.Strict  as Map

import           Test.Tasty       (testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))


test_Maps = [
    let map = Map.fromList [(2, "foo"), (3, "bar")] in testGroup "firstKey" [
      testCase "key exists" $ Maps.firstKey [1..] map @?= Just "foo"
    , testCase "no key" $ Maps.firstKey [4, 5] map @?= Nothing
    , testCase "empty list" $ Maps.firstKey [] map @?= Nothing
    ]
    , testCase "counts" $ Maps.counts [1, 2, 3, 1, 2, 4, 1] @?= Map.fromList [(1, 3), (2, 2), (3, 1), (4, 1)]
  ]
