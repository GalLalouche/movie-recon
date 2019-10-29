module Common.FoldablesTest where

import qualified Common.Foldables as F

import           Test.Tasty
import           Test.Tasty.HUnit


test_all = testGroup "Foldables" [
    testGroup "nth" [
      testCase "exists" $ F.nth 1 [1, 2, 3] @?= Just 2
    , testCase "does not exist" $ F.nth 4 [1, 2, 3] @?= Nothing
    , testCase "infinite" $ F.nth 4 [1..] @?= Just 5
    ]
  , let
      action = F.mapHeadOrElse (+ 1) 0
    in testGroup "mapHeadOrElse" [
      testCase "exists" $ action (Just 1) @?= 2
    , testCase "does not exist" $ action Nothing @?= 0
    ]
  , testCase "intercalate" $ F.intercalate ", " ["foo", "bar", "bazz"] @?= "foo, bar, bazz"
  , let
      map x = if x > 4 then Just x else Nothing
    in testGroup "mapFind" [
      testCase "exists" $ F.mapFind map [1..] @?= Just 5
    , testCase "does not exist" $ F.mapFind map [1..4] @?= Nothing
    , testCase "empty" $ F.mapFind map [] @?= Nothing
    ]
  , testGroup "average" [
      testCase "empty returns Nothing" $ F.average [] @?= Nothing
    , testCase "not empty returns just" $ F.average [1, 2, 3, 4] @?= Just 2.5
    ]
  ]
