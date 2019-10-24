{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MovieDB.ParserTest where

import qualified MovieDB.Parsers      as P
import           MovieDB.Types        (Movie(..), MovieId(..), ParticipationType(..), Person(..), PersonId(..))

import           Data.ByteString.Lazy (readFile)
import           Data.Text            (pack)
import           Data.Time            (fromGregorian)
import           Prelude              hiding (readFile)

import           Common.JsonUtils     (ObjectParser, parseObject)
import qualified Common.JsonUtils     as JU (decodeUnsafe, fromSuccess)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Common.TestCommon ((*?=))


parseJson :: ObjectParser a -> FilePath -> IO a
parseJson parser fileName = do
   json <- readFile $ "test/resources/MovieDB/" ++ fileName ++ ".json"
   return $ JU.fromSuccess $ parseObject parser (JU.decodeUnsafe json)

test_MovieDB_parsers = testGroup "parseJson" [
    testCase "parseMovieCredits" $ do
      res <- parseJson P.parseMovieCredits "movie_credits"
      let person id name role = (Person (PersonId $ pack $ show id) name, role)
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
      res <- parseJson P.parsePersonCredits "person_credits"
      let movie id name role y m d = (Movie (MovieId $ pack $ show id) name (fromGregorian y m d), role)
      let expected = [
              movie 268 "Batman" Actor 1989 6 23
            , movie 364 "Batman Returns" Actor 1992 6 19
            , movie 194662 "Birdman" Actor 2014 10 17
            , movie 24053 "The Merry Gentleman" Director 2008 4 16
            ]
      res *?= expected
  , testCase "parseName" $ do
      res <- parseJson P.parsePersonName "person"
      res @?= "Bradley Cooper"
  ]

test_URL_parsers = testGroup "parseId" [
    testCase "starting with HTTP" $ do
      let url = P.Url "https://www.themoviedb.org/person/1-george-lucas"
      P.parseId url @?= PersonId "1"
  , testCase "Not starting with HTTP" $ do
      let url = P.Url "themoviedb.org/person/2-mark-hamill"
      P.parseId url @?= PersonId "2"
  ]
