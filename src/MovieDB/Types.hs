{-# LANGUAGE DuplicateRecordFields #-}

module MovieDB.Types where

import Data.Text (Text)


newtype PersonId = PersonId { id :: Int } deriving (Show, Eq, Ord)
newtype MovieId = MovieId { id :: Text }
data Movie = Movie { id :: MovieId, name :: Text}
newtype Query = Query { query :: Text }
