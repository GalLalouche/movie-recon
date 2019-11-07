{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main.Action.Internal.FormatTest where

import qualified Data.Set                      as Set (fromList)
import           Data.Text                     (tail)
import qualified Data.Vector                   as Vector (fromList)
import           Prelude                       hiding (tail)
import           Text.InterpolatedString.Perl6 (qq)

import           MovieDB.Database.TestCommon   (makeMovie, makePerson)
import           MovieDB.Types                 (Participation(Participation), ParticipationType(Actor, Director, Writer))
import           OMDB                          (MovieScore(MovieScore), MovieScores(MovieScores), Source(IMDB, Metacritic, RottenTomatoes))

import           Main.Action.Internal.Format   (FullMovieInfo(FullMovieInfo), mkFullMovieInfoString, mkStringMovie)

import           Test.Tasty                    (testGroup)
import           Test.Tasty.HUnit              (testCase, (@?=))


movie = makeMovie "foobar" 42
score = Just $ MovieScores movie (Set.fromList [MovieScore IMDB 12, MovieScore RottenTomatoes 3, MovieScore Metacritic 84])
test_Format = [
    testGroup "mkStringMovie" [
      testCase "No score" $ mkStringMovie movie Nothing @?= "42\tfoobar\t2000-01-01\tNo score"
    , testCase "With score" $ mkStringMovie movie score @?= "42\tfoobar\t2000-01-01\t(12 IMDB, 3 RT, 84 MC)"
    ]
  , testCase "mkFullMovieInfoString" $ do
      let fmi = let actor1 = makePerson "actor1" 1
                    actor2 = makePerson "actor2" 2
                    director = makePerson "director" 4
                    writer = makePerson "writer" 5
                    participations = Vector.fromList [
                        Participation director movie Director
                      , Participation writer movie Writer
                      , Participation actor1 movie Actor
                      , Participation actor2 movie Actor
                      ]
                in FullMovieInfo movie participations score
      mkFullMovieInfoString fmi @?= let tab = "\t" :: String in tail [qq|
42{tab}foobar{tab}2000-01-01{tab}(12 IMDB, 3 RT, 84 MC)
{tab}director (Director)
{tab}writer (Writer)
{tab}actor1
{tab}actor2
|]
  ]
