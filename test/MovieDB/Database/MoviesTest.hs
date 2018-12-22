{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module MovieDB.Database.MoviesTest where

import MovieDB.Database.Movies (clear, init, insertOrVerify, getValue)

import Prelude hiding (init)

import MovieDB.Types (Movie(..), MovieId(..))

import MovieDB.Database.TestCommon (withTempDb)
import Test.Tasty
import Test.Tasty.HUnit

test_movie_database = [
    testCase "read after write" $ do
      let id = MovieId "foobar"
      res <- withTempDb $ do
        init
        insertOrVerify $ Movie id "moobar"
        getValue id
      res @=? (Just $ Movie id "moobar")
      return ()
  ]
