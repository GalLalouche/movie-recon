{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.ParticipationsTest where

import           Control.Monad.Trans.Maybe       (runMaybeT)
import qualified MovieDB.Database.Acted          as InstancesOnly
import           MovieDB.Database.Common         (DbCall)
import           MovieDB.Database.Participations
import           MovieDB.Types                   (Actor(..), ActorId(..), Movie(..), MovieId(..), PersonId)

import           MovieDB.Database.TestCommon     (unsafeMakeMovie, unsafeMakePerson, withTempDb)
import           Test.Tasty
import           Test.Tasty.HUnit


test_participations = [
    testCase "read after write" $ do
      let actor1 = unsafeMakePerson "actor1" :: Actor
      let actor2 = unsafeMakePerson "actor2" :: Actor
      let actor3 = unsafeMakePerson "actor3" :: Actor
      let movie1 = unsafeMakeMovie "movie1"
      let movie2 = unsafeMakeMovie "movie2"
      res <- withTempDb $ do
        addValueEntry movie1 actor1
        addValueEntry movie1 actor3
        addValueEntry movie2 actor2
        crew movie1 :: DbCall [Actor]
      res @=? [actor1, actor3]
      return ()
  ]
