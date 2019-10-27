{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Database.TestCommon (
  withTempDb,
  makePerson,
  makeMovie,
) where

import Data.Text                (Text, pack)
import Data.Time                (fromGregorian)

import MovieDB.Database         (DbCall, DbPath(..), runDbCall)
import MovieDB.Database.Movies  as Movies (init)
import MovieDB.Database.Persons as Persons (init)
import MovieDB.Types            (Movie(..), Person(..), mkMovieId, mkPersonId)


withTempDb :: DbCall a -> IO a
withTempDb a = runDbCall (initAll >> a) inMemoryDb where
  initAll = Movies.init >> Persons.init
  inMemoryDb = DbPath ""

makePerson :: Text -> Int -> Person
makePerson name id = Person (mkPersonId $ pack $ show id) name

makeMovie :: Text -> Int -> Movie
makeMovie name id = Movie (mkMovieId $ pack $ show id) name (fromGregorian 2000 1 1)
