{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.MoviesTest where

import qualified MovieDB.Database.Movies     as DB
import           MovieDB.Types               (Movie(..), mkMovieId)

import           Control.Monad.Trans.Maybe   (runMaybeT)
import           Data.Time                   (fromGregorian)

import           MovieDB.Database.TestCommon (withTempDb)
import           Test.Tasty.HUnit            (testCase, (@?=))


test_movie_database = [
    testCase "read after write" $ do
      let id = mkMovieId "42"
      let movie = Movie id "moobar" (fromGregorian 2000 1 1)
      res <- withTempDb $ do
        DB.init
        DB.insertOrVerify movie
        runMaybeT $ DB.getValue id
      res @?= Just movie
  ]
