{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IMDB where

import Entity hiding (Movie)
import Data.Text
import Control.Monad ((>=>))

newtype PersonId = PersonId { id :: String }
newtype MovieId = MovieId { id :: String }
data Movie = Movie { id :: MovieId, name :: Text}

movieIds :: PersonId -> IO [MovieId]
movieIds = undefined

movieInfo :: MovieId -> IO Movie
movieInfo = undefined

movies :: PersonId -> IO [Movie]
movies = movieIds >=> traverse movieInfo
