module Common.MaybesTest where

import Data.Char (toUpper)

import Common.Maybes     (check, fcheck, orMempty, mapMonoid)
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
  , testGroup "orMempty" [
      testCase "Nothing returns mempty" $ do
        let result = orMempty Nothing
        result @?= ""
      , testCase "false returns nothing" $ do
        let result = orMempty $ Just "foobar"
        result @?= "foobar"
    ]
  , testGroup "mapMonoid" [
      testCase "Nothing returns mempty" $ do
        let result = mapMonoid (const undefined) Nothing
        result @?= ""
      , testCase "false returns nothing" $ do
        let result = mapMonoid show (Just 4)
        result @?= "4"
    ]
  ]
