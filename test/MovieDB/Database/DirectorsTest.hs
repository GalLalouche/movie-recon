{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module MovieDB.Database.DirectorsTest where

import MovieDB.Database.Directors (clear, init, write, read)

import Prelude hiding (read, init)

import MovieDB.Types (Director(..), DirectorId(..), PersonId(..))

import MovieDB.Database.TestCommon (withTempDb)
import Test.Tasty
import Test.Tasty.HUnit

test_director_database = [
    testCase "read after write" $ do
      let id = DirectorId $ PersonId "foobar"
      res <- withTempDb $ do
        init
        write $ Director id "moobar"
        read id
      res @=? (Just $ Director id "moobar")
      return ()
  ]
