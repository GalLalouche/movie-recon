{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module MovieDB.Database.CastAndCrewsTest where

import Data.Monoid ((<>))
import Data.Text (Text)
import MovieDB.Database.CastAndCrews (clear, init, insertOrVerify, getValue)
import MovieDB.Types
import Control.Monad.Trans.Maybe (runMaybeT)

import Prelude hiding (init)


import MovieDB.Database.TestCommon (withTempDb)
import Test.Tasty
import Test.Tasty.HUnit

make :: Person p => Text -> p
make name = makePerson (PersonId (name <> "Id")) name

test_castAndCrew_database = [
    testCase "read after write" $ do
      let movie = Movie (MovieId "movieId") "movie"
      let director = make "director"
      let writer = make "writer"
      let actors = [make "actor1", make "actor2"]
      let value = CastAndCrew movie director writer actors
      res <- withTempDb $ do
        init
        insertOrVerify value
        runMaybeT $ getValue $ CastAndCrewId $ MovieId "movieId"
      res @=? Just value
  ]
