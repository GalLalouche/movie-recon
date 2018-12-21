{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module MovieDB.Database.MoviesTest where

import MovieDB.Database.Movies (clear, init, write, read)
import MovieDB.Database.Common (DbCall, DbPath(..))

import Prelude hiding (read, init)

import Data.Text (Text)
import System.Directory (removeFile)
import MovieDB.Types (Movie(..), MovieId(..))

import Control.Monad.Trans.Reader (runReaderT)

import Test.Tasty
import Test.Tasty.HUnit

-- Since shared in memory doesn't work :\
withDbPath :: DbCall a -> IO a
withDbPath call = do
  result <- runReaderT call (DbPath "temp_file")
  removeFile "temp_file"
  return result

test_database = [
    testCase "read after write" $ do
      let id = MovieId "foobar"
      res <- withDbPath $ do
        init
        write $ Movie id "moobar"
        read id
      res @=? (Just $ Movie id "moobar")
      return ()
  ]
