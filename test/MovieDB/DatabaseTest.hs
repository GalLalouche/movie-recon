{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module MovieDB.DatabaseTest where

import Prelude hiding (read, init)

import Common.Unsafe
import Data.Text (Text)
import System.Directory (removeFile)
import MovieDB.Database
import MovieDB.Types

import Control.Monad.Trans.Reader

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
        clear
        init
        write $ Movie id "moobar"
        read id
      res @=? (Just $ Movie id "moobar")
      return ()
  ]
