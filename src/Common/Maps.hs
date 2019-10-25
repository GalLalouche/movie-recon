module Common.Maps where

import           Data.Foldable    (foldl')
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (fromMaybe)
import           Data.Semigroup   (Semigroup, (<>))

import           Control.Arrow    ((&&&))

import           Common.Operators
import           Common.Foldables (mapFind)


mapBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k a
mapBy = flip fromFoldable id

fromFoldable :: (Ord k, Foldable f) => (a -> k) -> (a -> v) -> f a -> Map k v
fromFoldable extractKey extractValue = foldl' append Map.empty where
  append = flip $ uncurry Map.insert . (extractKey &&& extractValue)

multiMapBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k [a]
multiMapBy = flip semigroupMapBy pure

upsert :: (Ord k, Semigroup a) => k -> a -> Map k a -> Map k a
upsert = aggUpsert (<>)

aggUpsert :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
aggUpsert agg key value m = let newValue = Map.lookup key m |> maybe value (agg value) in Map.insert key newValue m

semigroupMapBy :: (Ord k, Foldable f, Semigroup v) => (a -> k) -> (a -> v) -> f a -> Map k v
semigroupMapBy = aggMapBy (<>)

aggMapBy :: (Ord k, Foldable f) => (v -> v -> v) -> (a -> k) -> (a -> v) -> f a -> Map k v
aggMapBy agg extractKey extractValue = foldl' append Map.empty where
  append = flip $ uncurry (aggUpsert agg) . (extractKey &&& extractValue)

monoidLookup :: (Ord k, Monoid a) => k -> Map k a -> a
monoidLookup = fromMaybe mempty .: Map.lookup

counts :: (Foldable f, Ord a) => f a -> Map a Int
counts = aggMapBy (+) id (const 1)

firstKey :: (Foldable f, Ord k) => f k -> Map k v -> Maybe v
firstKey fa = flip mapFind fa . (Map.!?)
