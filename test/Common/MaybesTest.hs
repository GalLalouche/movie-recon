module Common.MaybesTest where

import Common.Maybes     (check, fcheck)
import Common.TestCommon (Box(..))

import Test.Tasty
import Test.Tasty.HUnit


test_all = testGroup "Maybes"  [
    testGroup "check" [
      testCase "true returns just" $ do
        let result = check (> 0) 1
        result @?= Just 1
      , testCase "false returns nothing" $ do
        let result = check (< 0) 1
        result @?= Nothing
    ]
  , testGroup "fcheck" [
      testCase "true returns just" $ do
        let result = fcheck (\a -> Box $ a > 0) 1
        result @?= Box (Just 1)
      , testCase "false returns nothing" $ do
        let result = fcheck (\a -> Box $ a < 0) 1
        result @?= Box Nothing
    ]
  ]
