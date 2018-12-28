module Common.Maps where

import           Data.Map.Lazy    (Map)
import qualified Data.Map.Lazy    as M
import           Data.Maybe       (fromMaybe)
import           Data.Semigroup   (Semigroup, (<>))
import qualified Data.Semigroup   as SG

import           Common.Operators

mapBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k a
mapBy extractKey = foldr append M.empty where
  append a = let key = extractKey a in M.insert key a

multiMapBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k [a]
multiMapBy extractKey = semigroupMapBy extractKey pure

upsert :: (Ord k, Semigroup a) => k -> a -> Map k a -> Map k a
upsert key value m = let newValue = M.lookup key m |> maybe value (value <>)  in M.insert key newValue m

semigroupMapBy :: (Ord k, Foldable f, Semigroup v) => (a -> k) -> (a -> v) -> f a -> Map k v
semigroupMapBy extractKey extractValue = foldr append M.empty where
  append a = let
      key = extractKey a
      value = extractValue a
    in upsert key value

monoidLookup :: (Ord k, Monoid a) => k -> Map k a -> a
monoidLookup k = fromMaybe mempty . M.lookup k
