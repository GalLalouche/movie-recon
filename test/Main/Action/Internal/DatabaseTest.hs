{-# LANGUAGE OverloadedStrings #-}

module Main.Action.Internal.DatabaseTest where

import           MovieDB.Types                 (FilterReason(Ignored, Seen), mkMovieId)

import qualified Main.Action.Internal.Database as DB

import           Common.TestCommon             (assertThrows)
import           Test.Tasty                    (testGroup)
import           Test.Tasty.HUnit              (testCase, (@?=))


test_MainActionInternalDatabase = [
    testGroup "parseSeenMovieLine" [
      testCase "seen" $ DB.parseSeenMovieLine "S1234\tblah blah" @?=(mkMovieId "1234", Seen)
    , testCase "ignored" $ DB.parseSeenMovieLine "I1234\tblu blu" @?= (mkMovieId "1234", Ignored)
    , testCase "invalid prefix" $ assertThrows $ snd $ DB.parseSeenMovieLine "K1234"
    ]
  ]
