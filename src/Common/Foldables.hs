module Common.Foldables where

import Data.Foldable (null, toList)
import Data.Maybe    (fromJust)
import Data.Monoid   ((<>), First(..))


notNull :: Foldable f => f a -> Bool
notNull = not . null

headOpt :: Foldable f => f a -> Maybe a
headOpt = nth 0

headUnsafe :: Foldable f => f a -> a
headUnsafe = fromJust . headOpt

nth :: Foldable f => Int -> f a -> Maybe a
nth i = aux i . toList where
  aux _ []       = Nothing
  aux 0 (x : _)  = Just x
  aux n (_ : xs) = aux (n - 1) xs

mapHeadOrElse :: Foldable f => (a -> b) -> b -> f a -> b
mapHeadOrElse f def fa = case headOpt fa of
  Nothing  -> def
  (Just x) -> f x

-- Copy pasted from https://hackage.haskell.org/package/bytestring-tree-builder-0.2.7.3/docs/src/ByteString.TreeBuilder.html#intercalate
intercalate :: (Foldable f, Monoid m) => m -> f m -> m
intercalate incut = aux . toList where
  aux []           = mempty
  aux [a]          = a
  aux (x : xs) = x <> incut <> aux xs

mapFind :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
mapFind f = getFirst . foldMap (First . f)
