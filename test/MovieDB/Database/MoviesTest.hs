{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module MovieDB.Database.MoviesTest where

import MovieDB.Database.Movies     (clear, getValue, init, insertOrVerify)
import MovieDB.Types               (Movie(..), MovieId(..))

import Control.Monad.Trans.Maybe   (runMaybeT)
import Data.Time                   (fromGregorian)

import Prelude                     hiding (init)

import MovieDB.Database.TestCommon (withTempDb)

import Test.Tasty
import Test.Tasty.HUnit

test_movie_database = [
    testCase "read after write" $ do
      let id = MovieId "foobar"
      res <- withTempDb $ do
        init
        insertOrVerify $ Movie id "moobar" (fromGregorian 2000 1 1)
        runMaybeT $ getValue id
      res @?= (Just $ Movie id "moobar" (fromGregorian 2000 1 1))
  ]
