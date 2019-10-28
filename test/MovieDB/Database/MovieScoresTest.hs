{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.MovieScoresTest where

import           Control.Monad.Trans.Maybe    (runMaybeT)
import qualified Data.Set                     as Set (fromList)
import           Data.Time                    (fromGregorian)

import           MovieDB.Types                (Movie(..), mkMovieId)
import           OMDB                         (MovieScore(..), MovieScores(..), Source(IMDB, Metacritic, RottenTomatoes))

import qualified MovieDB.Database.MovieScores as DB

import           Common.TestCommon            ((*?=))
import           MovieDB.Database.TestCommon  (withTempDb)
import           Test.Tasty                   (testGroup)
import           Test.Tasty.HUnit             (testCase, (@?=))


movie = Movie (mkMovieId "42") "foobar" (fromGregorian 2000 1 1)
movie2 = Movie (mkMovieId "54") "bazz" (fromGregorian 1999 1 1)
movieScores = MovieScores movie (Set.fromList [MovieScore IMDB 12, MovieScore RottenTomatoes 3, MovieScore Metacritic 84])
movieScores2 = MovieScores movie2 (Set.fromList [MovieScore IMDB 90, MovieScore RottenTomatoes 60])

testMovieScores = testGroup "movieScores" [
      testCase "No movies returns Nothing" $ do
        res <- initializeAndGetScores movie2
        res @?= Nothing
    , testCase "has scores returns Just scores" $ do
        (Just (MovieScores m scores)) <- initializeAndGetScores movie
        m @?= movie
        scores *?= _scores movieScores
  ] where
    initializeAndGetScores m = withTempDb $ DB.init >> DB.addMovieScores movieScores >> runMaybeT (DB.movieScores m)

testAllMovieScores = testGroup "allMovieScores" [
      testCase "No movies returns an empty list" $ do
        res <- withTempDb $ DB.init >> DB.allMovieScores
        res *?= []
    , testCase "returns movies" $ do
        res <- withTempDb $ DB.init >> DB.addMovieScores movieScores >> DB.addMovieScores movieScores2 >> DB.allMovieScores
        res *?= [movieScores2, movieScores]
  ]

test_MovieScores = [
    testMovieScores
  , testAllMovieScores
  ]
