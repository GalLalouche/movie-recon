module Common.Sets where

import           Data.Foldable (toList)
import           Data.Set      (Set, fromList, map)
import qualified Data.Set      as Set


from :: (Foldable f, Ord a) => f a -> Set a
from = fromList . toList

(<$>) :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
(<$>) = Set.map
infixl 4 <$> -- Same priority as Prelude.<$>

(<$$>) :: (Ord a, Ord b) => Set a -> (a -> b) -> Set b
(<$$>) = flip Set.map
infixl 1 <$$> -- Same priority as Common.Operators.<$$>

(>$>) :: (Ord a, Ord b, Ord c) => (a -> Set b) -> (b -> c) -> (a -> Set c)
(f >$> g) a = f a <$$> g

(<$<) :: (Ord a, Ord b, Ord c) => (b -> c) -> (a -> Set b) -> (a -> Set c)
(<$<) = flip (>$>)
infixr 1 >$>, <$< -- Same priority Operators equivalent
