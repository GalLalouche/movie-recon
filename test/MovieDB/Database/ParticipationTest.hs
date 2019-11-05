{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.ParticipationTest where

import           MovieDB.Database.Movie         ()
import qualified MovieDB.Database.Participation as DB
import           MovieDB.Types                  (CastAndCrew(actors, directors, writers), Participation(Participation), ParticipationType(Actor, Director, Writer))

import           Common.MaybeTs                 (fromJust)

import           Common.TestCommon              ((*?=))
import           MovieDB.Database.TestCommon    (makeMovie, makePerson, withTempDb)
import           Test.Tasty                     (testGroup)
import           Test.Tasty.HUnit               (testCase, (@?=))


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
        _ <- DB.init
        _ <- DB.addValueEntry $ Participation director movie1 Director
        _ <- DB.addValueEntry $ Participation writer movie2 Writer
        _ <- DB.addValueEntry $ Participation actor1 movie1 Actor
        _ <- DB.addValueEntry $ Participation actor2 movie2 Actor
        _ <- DB.addValueEntry $ Participation actor3 movie1 Actor
        fromJust $ DB.castAndCrew movie1
      directors res *?= [director]
      writers res *?= []
      actors res *?= [actor1, actor3]
      return ()
  , let
        person1 = makePerson "actor1" 1
        person2 = makePerson "actor2" 2
        movie1 = makeMovie "movie1" 3
        movie2 = makeMovie "movie2" 4
        init = DB.init >> DB.addValueEntry (Participation person1 movie1 Actor)
    in testGroup "hasParticipated" [
      testCase "participated" $ withTempDb (init >> DB.hasParticipated person1 movie1) >>= (DB.Participated @?=)
    , testCase "did not participated" $ withTempDb (init >> DB.hasParticipated person2 movie1) >>= (DB.DidNotParticipate @?=)
    , testCase "unknown1" $ withTempDb (init >> DB.hasParticipated person1 movie2) >>= (DB.Unknown @?=)
    , testCase "unknown2" $ withTempDb (init >> DB.hasParticipated person2 movie2) >>= (DB.Unknown @?=)
    ]
  ]
