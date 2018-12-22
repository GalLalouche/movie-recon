{-# LANGUAGE DuplicateRecordFields #-}

module MovieDB.Types where

import Prelude hiding (id)
import Control.Lens.TH
import Data.Text (Text)


newtype PersonId = PersonId { _id :: Text } deriving (Show, Eq, Ord)

newtype DirectorId = DirectorId { _id :: PersonId } deriving (Show, Eq, Ord)
data Director = Director { _id :: DirectorId, _name :: Text } deriving (Show, Eq, Ord)

newtype MovieId = MovieId { _id :: Text } deriving (Show, Eq, Ord)
data Movie = Movie { _id :: MovieId, _name :: Text} deriving (Show, Eq, Ord)

newtype Query = Query { query :: Text }
