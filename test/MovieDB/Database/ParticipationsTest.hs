{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.ParticipationsTest where

import MovieDB.Database.Movies         ()
import MovieDB.Database.Participations
import MovieDB.Types                   (CastAndCrew(..), Participation(..), ParticipationType(Actor, Director, Writer))

import Common.MaybeTUtils              (fromJust)

import Common.TestCommon               ((*?=))
import MovieDB.Database.TestCommon     (makeMovie, makePerson, withTempDb)
import Test.Tasty
import Test.Tasty.HUnit


test_participations = [
    testCase "read after write" $ do
      let actor1 = makePerson "actor1"
      let actor2 = makePerson "actor2"
      let actor3 = makePerson "actor3"
      let director = makePerson "director"
      let writer = makePerson "director"
      let movie1 = makeMovie "movie1"
      let movie2 = makeMovie "movie2"
      res <- withTempDb $ do
        addValueEntry $ Participation director movie1 Director
        addValueEntry $ Participation writer movie2 Writer
        addValueEntry $ Participation actor1 movie1 Actor
        addValueEntry $ Participation actor2 movie2 Actor
        addValueEntry $ Participation actor3 movie1 Actor
        fromJust $ castAndCrew movie1
      directors res @?= [director]
      writers res @?= []
      actors res *?= [actor1, actor3]
      return ()
  ]
