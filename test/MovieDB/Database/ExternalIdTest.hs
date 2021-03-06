{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.ExternalIdTest where

import           Data.Time                   (fromGregorian)
import           Prelude                     hiding (init)

import           MovieDB.Types               (ExternalHost(IMDB), Movie(Movie), mkImdbId, mkMovieId, toExternalId)

import           MovieDB.Database            (Nullable(NoRow, NotNull, Null))
import qualified MovieDB.Database.ExternalId as EI

import           MovieDB.Database.TestCommon (withTempDb)
import           Test.Tasty.HUnit            (testCase, (@=?))


movie1 = Movie (mkMovieId "42") "foobar" (fromGregorian 2000 1 1)
movie2 = Movie (mkMovieId "54") "bazz" (fromGregorian 1999 1 1)
movie3 = Movie (mkMovieId "1234") "quxx" (fromGregorian 1999 1 1)

externalId = toExternalId movie1 (mkImdbId "tt1234")
setupAndGet m = withTempDb $ EI.init >> EI.addExternalId externalId >> EI.addNullExternalId movie2 IMDB >> EI.externalId IMDB m

test_ExternalId = [
    testCase "No row" $ setupAndGet movie3 >>= (NoRow @=?)
  , testCase "Null ID" $ setupAndGet movie2 >>= (Null @=?)
  , testCase "NotNull ID" $ setupAndGet movie1 >>= (NotNull externalId @=?)
  ]
