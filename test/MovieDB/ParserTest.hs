{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module MovieDB.ParserTest where

import qualified MovieDB.Parsers         as P
import           MovieDB.Types           (Movie(..), MovieId(..), Participation(..), ParticipationType(..),
                                          Person(..), PersonId(..))

import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Types        as AesonT

import           Data.ByteString.Lazy    (readFile)
import           Data.Set                (fromList)
import           Data.Text               (Text, pack)
import           Data.Time               (fromGregorian)
import           Prelude                 hiding (readFile)

import           Common.JsonObjectParser (ObjectParser, parseObject)
import           Common.JsonUtils        (decodeUnsafe, fromSuccess)

import           Test.Tasty
import           Test.Tasty.HUnit


parseJson :: ObjectParser a -> FilePath -> IO a
parseJson parser fileName = do
   json <- readFile $ "test/resources/MovieDB/Database/" ++ fileName ++ ".json"
   return $ fromSuccess $ parseObject parser (decodeUnsafe json)

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
      fromList res @?= fromList expected
  , testCase "parsePersonCredits" $ do
      res <- parseJson P.parsePersonCredits "person_credits"
      let movie id name role y m d = (Movie (MovieId $ pack $ show id) name (fromGregorian y m d), role)
      let expected = [
              movie 268 "Batman" Actor 1989 6 23
            , movie 364 "Batman Returns" Actor 1992 6 19
            , movie 194662 "Birdman" Actor 2014 10 17
            , movie 24053 "The Merry Gentleman" Director 2008 4 16
            ]
      fromList res @?= fromList expected
  ]
