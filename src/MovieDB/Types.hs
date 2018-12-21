{-# LANGUAGE DuplicateRecordFields #-}

module MovieDB.Types where

import Data.Text (Text)


newtype PersonId = PersonId { id :: Text } deriving (Show, Eq, Ord)
newtype DirectorId = DirectorId { id:: PersonId } deriving (Show, Eq, Ord)
data Director = Director { id:: DirectorId, name :: Text } deriving (Show, Eq, Ord)


newtype MovieId = MovieId { id :: Text } deriving (Show, Eq, Ord)
data Movie = Movie { id :: MovieId, name :: Text} deriving (Show, Eq, Ord)

newtype Query = Query { query :: Text }
