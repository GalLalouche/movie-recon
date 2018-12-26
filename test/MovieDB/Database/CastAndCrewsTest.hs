{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.CastAndCrewsTest where

import Control.Monad.Trans.Maybe     (runMaybeT)
import MovieDB.Database.CastAndCrews (addCastAndCrew, getCastAndCrew)
import MovieDB.Types                 (CastAndCrew(..), Movie(..))

import MovieDB.Database.TestCommon   (unsafeMakeMovie, unsafeMakePerson, withTempDb)
import Test.Tasty
import Test.Tasty.HUnit

test_castAndCrew_database =
  [ testCase "read after write" $ do
      let movie = unsafeMakeMovie "movie"
      let director = [unsafeMakePerson "director"]
      let writers = [unsafeMakePerson "writer1", unsafeMakePerson "writer2"]
      let actors = [unsafeMakePerson "actor1", unsafeMakePerson "actor2"]
      let value = CastAndCrew movie director writers actors
      res <- withTempDb $ do
        addCastAndCrew value
        runMaybeT $ getCastAndCrew $ _id movie
      res @=? Just value
  ]
