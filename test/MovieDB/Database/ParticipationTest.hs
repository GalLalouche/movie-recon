{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.ParticipationTest where

import           Data.Vector                     (Vector)
import qualified Data.Vector                     as Vector (fromList)

import           MovieDB.Database.Movie         ()
import qualified MovieDB.Database.Participation as DB
import           MovieDB.Types                   (CastAndCrew(..), Participation(..), ParticipationType(Actor, Director, Writer))

import           Common.MaybeTs                  (fromJust)

import           Common.TestCommon               ((*?=))
import           MovieDB.Database.TestCommon     (makeMovie, makePerson, withTempDb)
import           Test.Tasty.HUnit                (testCase, (@?=))


test_Participation = [
    testCase "read after write" $ do
      let actor1 = makePerson "actor1" 1
      let actor2 = makePerson "actor2" 2
      let actor3 = makePerson "actor3" 3
      let director = makePerson "director" 4
      let writer = makePerson "director" 5
      let movie1 = makeMovie "movie1" 6
      let movie2 = makeMovie "movie2" 7
      res <- withTempDb $ do
        DB.init
        DB.addValueEntry $ Participation director movie1 Director
        DB.addValueEntry $ Participation writer movie2 Writer
        DB.addValueEntry $ Participation actor1 movie1 Actor
        DB.addValueEntry $ Participation actor2 movie2 Actor
        DB.addValueEntry $ Participation actor3 movie1 Actor
        fromJust $ DB.castAndCrew movie1
      directors res *?= [director]
      writers res *?= []
      actors res *?= [actor1, actor3]
      return ()
  ]
