module Common.Foldables where

import Data.Foldable (null, toList)
import Data.Map      (Map)
import Data.Maybe    (fromJust)

import Common.Maps   (aggMapBy)


notNull :: Foldable f => f a -> Bool
notNull = not . null

headOpt :: Foldable f => f a -> Maybe a
headOpt = nth 0

headUnsafe :: Foldable f => f a -> a
headUnsafe = fromJust . headOpt

nth :: Foldable f => Int -> f a -> Maybe a
nth i = aux i . toList where
  aux _ []       = Nothing
  aux 0 (x : _) = Just x
  aux n (_ : xs) = aux (n - 1) xs

mapHeadOrElse :: Foldable f => (a -> b) -> b -> f a -> b
mapHeadOrElse f def foldable = case headOpt foldable of
  Nothing  -> def
  (Just x) -> f x

counts :: (Foldable f, Ord a) => f a -> Map a Int
counts = aggMapBy (+) id (const 1)
