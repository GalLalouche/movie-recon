module Common.FoldablesTest where

import qualified Common.Foldables as F

import           Test.Tasty
import           Test.Tasty.HUnit


test_all = testGroup "Foldables" [
    testGroup "nth" [
        testCase "exists" $ do
          let result = F.nth 1 [1, 2, 3]
          result @?= Just 2
      , testCase "does not exist" $ do
          let result = F.nth 4 [1, 2, 3]
          result @?= Nothing
    ]
  , let
      action = F.mapHeadOrElse (+ 1) 0
    in testGroup "mapHeadOrElse" [
        testCase "exists" $ do
          let result = action $ Just 1
          result @?= 2
      , testCase "does not exist" $ do
          let result = action Nothing
          result @?= 0
    ]
  ]
