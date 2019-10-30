{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module MovieDB.API.InternalTest where

import           API                  (Url(..))
import qualified MovieDB.API.Internal as I
import           MovieDB.Types        (pattern ImdbId, Movie(..), ParticipationType(Actor, Director, Writer), Person(..), mkMovieId, mkPersonId)

import           Data.Text            (pack)
import           Data.Time            (fromGregorian)

import           Common.JsonTestUtils (parseJson)
import           Common.TestCommon    ((*?=))
import           Test.Tasty
import           Test.Tasty.HUnit


test_MovieDB_Internal = [
    testGroup "parseJson" [
      testCase "parseMovieCredits" $ do
        res <- parseJson I.parseMovieCredits "MovieDB/movie_credits"
        let person id name role = (Person (mkPersonId $ pack $ show id) name, role)
        let expected = [
                person 2232 "Michael Keaton" Actor
              , person 819 "Edward Norton" Actor
              , person 54693 "Emma Stone" Actor
              , person 223 "Alejandro González Iñárritu" Director
              , person 223 "Alejandro González Iñárritu" Writer
              , person 1281196 "Alexander Dinelaris" Writer
              , person 661870 "Armando Bo" Writer
              ]
        res *?= expected
    , testCase "parsePersonCredits" $ do
        res <- parseJson I.parsePersonCredits "MovieDB/person_credits"
        let movie id name role y m d = (Movie (mkMovieId $ pack $ show id) name (fromGregorian y m d), role)
        let expected = [
                movie 268 "Batman" Actor 1989 6 23
              , movie 364 "Batman Returns" Actor 1992 6 19
              , movie 194662 "Birdman" Actor 2014 10 17
              , movie 24053 "The Merry Gentleman" Director 2008 4 16
              ]
        res *?= expected
    , testCase "parseName" $ ("Bradley Cooper" @=?) =<< parseJson I.parsePersonName "MovieDB/person"
    , testGroup "parseImdbId" [
        testCase "has ID" $ do
          Just (ImdbId res) <- parseJson I.parseImdbId "MovieDB/external_ids"
          res @?= "tt0368226"
      , testCase "No ID" $ (Nothing @=?) =<< parseJson I.parseImdbId "MovieDB/external_ids_no_id"
      , testCase "empty ID" $ (Nothing @=?) =<< parseJson I.parseImdbId "MovieDB/external_ids_empty"
      ]
    ]
    , testGroup "parseId" [
        testCase "starting with HTTP" $ I.parseId (Url "https://www.themoviedb.org/person/1-george-lucas") @?= mkPersonId "1"
      , testCase "Not starting with HTTP" $ I.parseId (Url "themoviedb.org/person/2-mark-hamill") @?= mkPersonId "2"
      ]
  ]
