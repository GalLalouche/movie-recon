{-# LANGUAGE OverloadedStrings   #-}

module MovieDB.Database.MoviesTest where

import MovieDB.Database.Movies     (getValue, init, insertOrVerify)
import MovieDB.Types               (Movie(..), mkMovieId)

import Control.Monad.Trans.Maybe   (runMaybeT)
import Data.Time                   (fromGregorian)

import Prelude                     hiding (init)

import MovieDB.Database.TestCommon (withTempDb)
import Test.Tasty
import Test.Tasty.HUnit


test_movie_database = [
    testCase "read after write" $ do
      let id = mkMovieId "42"
      let movie = Movie id "moobar" (fromGregorian 2000 1 1)
      res <- withTempDb $ do
        init
        insertOrVerify movie
        runMaybeT $ getValue id
      res @?= Just movie
  ]
