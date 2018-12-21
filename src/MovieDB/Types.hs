{-# LANGUAGE DuplicateRecordFields, TemplateHaskell, FunctionalDependencies #-}


module MovieDB.Types where

import Control.Lens.TH
import Data.Text (Text)


newtype PersonId = PersonId { _id :: Text } deriving (Show, Eq, Ord)
newtype DirectorId = DirectorId { _id:: PersonId } deriving (Show, Eq, Ord)
data Director = Director { _id:: DirectorId, _name :: Text } deriving (Show, Eq, Ord)


newtype MovieId = MovieId { id :: Text } deriving (Show, Eq, Ord)
data Movie = Movie { id :: MovieId, name :: Text} deriving (Show, Eq, Ord)

newtype Query = Query { query :: Text }
