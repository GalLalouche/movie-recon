{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module MovieDB.Database.MoviesTest where

import MovieDB.Database.Movies (clear, init, insertOrVerify, getValue)
import MovieDB.Types (Movie(..), MovieId(..))
import Control.Monad.Trans.Maybe (runMaybeT)

import Prelude hiding (init)


import MovieDB.Database.TestCommon (withTempDb)
import Test.Tasty
import Test.Tasty.HUnit

test_movie_database = [
    testCase "read after write" $ do
      let id = MovieId "foobar"
      res <- withTempDb $ do
        init
        insertOrVerify $ Movie id "moobar"
        runMaybeT $ getValue id
      res @=? (Just $ Movie id "moobar")
      return ()
  ]
