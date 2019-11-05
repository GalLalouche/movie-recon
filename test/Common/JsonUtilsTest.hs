{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Common.JsonUtilsTest where

import           Common.JsonUtils  ((\>), (\\))
import qualified Common.JsonUtils  as JU
import           Common.TestCommon (assertThrows)

import           Data.Aeson.Types  (Value(Number))
import           Data.Text         (Text)
import           Data.Time         (fromGregorian)

import           Test.Tasty        (testGroup)
import           Test.Tasty.HUnit  (testCase, (@?=))
import           Text.RawString.QQ (r)


parse :: JU.ObjectParser a -> Text -> a
parse p t = JU.fromSuccess $ JU.parseObject p $ JU.decodeUnsafe t

test_all = testGroup "JsonUtils"  [
    testCase "int" $ do
      let result = parse (JU.int "foo") [r|{"foo": 42}|]
      result @?= 42
  , testCase "str" $ do
      let result = parse (JU.str "foo") [r|{"foo": "bar"}|]
      result @?= "bar"
  , testGroup "strMaybe" [
      testCase "exists returns Just" $ do
        let result = parse (JU.strMaybe "foo") [r|{"foo": "bar"}|]
        result @?= Just "bar"
    , testCase "not exists returns Nothing" $ do
        let result = parse (JU.strMaybe "bazz") [r|{"foo": "bar"}|]
        result @?= Nothing
    , testCase "Not string throws" $ assertThrows $ parse (JU.strMaybe "foo") [r|{"foo": 5}|]
    ]
  , testCase "read" $ do
      let result = parse (JU.strRead "foo") [r|{"foo": "2018-01-01"}|]
      result @?= fromGregorian 2018 1 1
  , testGroup "array" [
      testCase "valid" $ do
        let result = parse (JU.array "foo") [r|{"foo": [1, 2, 3, 4]}|]
        result @?= [Number 1, Number 2, Number 3, Number 4]
    , testCase "no such field throws" $ assertThrows $ parse (JU.array "bazz") [r|{"foo": [1, 2, 3, 4]}|]
    , testCase "invalid type throws" $ assertThrows $ parse (JU.array "foo") [r|{"foo": "bar"}|]
    ]
  , testCase "combiners" $ do
      let result = parse ("foo" \\ "bar" \> "bazz") [r|{"foo": {"bar": {"bazz": 42}}}|] :: Int
      result @?= 42
  , testGroup "withObjects" [
      testCase "valid" $ do
        let result = parse (JU.withObjects "foo" (JU.int "bar")) [r|{"foo": [{"bar": 1}, {"bar": 2}, {"bar": 3}]}|]
        result @?= [1, 2, 3]
    , testCase "invalid field throws" $ assertThrows $ parse (JU.withObjects "foo" (JU.int "bar")) [r|{"foo": 5}|]
    , testCase "no fieled throws" $ assertThrows $ parse (JU.withObjects "bazz" (JU.int "bar")) [r|{"foo": 5}|]
    ]
  ]
