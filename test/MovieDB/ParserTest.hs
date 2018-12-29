{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module MovieDB.ParserTest where

import qualified MovieDB.Parsers      as P
import           MovieDB.Types        (Participation(..), ParticipationType(..), Person(..), PersonId(..))

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as AesonT

import           Data.ByteString.Lazy (readFile)
import           Data.Set             (fromList)
import           Data.Text            (Text, pack)
import           Prelude              hiding (readFile)

import           Common.JsonUtils     (fromSuccess)
import           Common.Unsafe        (right)

import           Test.Tasty
import           Test.Tasty.HUnit


test_MovieDB_parsers = testGroup "parseJson" [
    testCase "parseMovieCredits" $ do
      json <- readFile "test/resources/MovieDB/Database/movie_credits.json"
      let res = fromSuccess $ AesonT.parse P.parseMovieCredits (right $ Aeson.eitherDecode json)
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
  ]
