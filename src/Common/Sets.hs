module Common.Sets where

import Data.Foldable (toList)
import Data.Set      (Set, fromList)


from :: (Foldable f, Ord a) => f a -> Set a
from = fromList . toList
