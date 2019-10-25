{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.MovieScoresTest where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Maybe    (runMaybeT)
import qualified Data.Set                     as Set (fromList)
import           Data.Time                    (fromGregorian)
import           Prelude                      hiding (init)

import           MovieDB.Types                (Movie(..), MovieId(..))
import           OMDB                         (MovieScore(..), MovieScores(..), Source(IMDB, Metacritic, RottenTomatoes))

import           MovieDB.Database.Common      (DbCall)
import qualified MovieDB.Database.Movies      ()
import qualified MovieDB.Database.MovieScores as MS
import           MovieDB.Database.TestCommon  (withTempDb)

import           Common.TestCommon            ((*?=))
import           Test.Tasty
import           Test.Tasty.HUnit

movie = Movie (MovieId "42") "foobar" (fromGregorian 2000 1 1)
movie2 = Movie (MovieId "54") "bazz" (fromGregorian 1999 1 1)
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
    initializeAndGetScores m = withTempDb $ MS.init >> MS.addMovieScores movieScores >> runMaybeT (MS.movieScores m)

testAllMovieScores = testGroup "allMovieScores" [
      testCase "No movies returns an empty list" $ do
        res <- withTempDb $ MS.init >> MS.allMovieScores
        res @?= []
    , testCase "returns movies" $ do
        res <- withTempDb $ MS.init >> MS.addMovieScores movieScores >> MS.addMovieScores movieScores2 >> MS.allMovieScores
        res *?= [movieScores2, movieScores]
  ]

test_MovieScores = [
    testMovieScores
  , testAllMovieScores
  ]
