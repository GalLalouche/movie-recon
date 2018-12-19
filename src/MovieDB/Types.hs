{-# LANGUAGE DuplicateRecordFields #-}

module MovieDB.Types where

import Data.Text (Text)


newtype PersonId = PersonId { id :: Int } deriving (Show, Eq, Ord)
newtype MovieId = MovieId { id :: Text } deriving (Show, Eq, Ord)
data Movie = Movie { id :: MovieId, name :: Text} deriving (Show, Eq, Ord)
newtype Query = Query { query :: Text }
