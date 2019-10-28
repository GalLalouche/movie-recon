module Common.Vectors where

import           Data.Foldable                (toList)
import           Data.Function                (on)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import qualified Data.Vector.Algorithms.Intro as Algo (sort, sortBy)


sort :: Ord a => Vector a -> Vector a
sort = V.modify Algo.sort

sortOn :: Ord b => (a -> b) -> Vector a -> Vector a
sortOn f = V.modify (Algo.sortBy (compare `on` f))

from :: Foldable f => f a -> Vector a
from = V.fromList . toList
