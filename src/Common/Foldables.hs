module Common.Foldables where

import Data.Foldable    (null, toList)
import Data.Maybe       (fromJust, maybe)
import Data.Monoid      (First(First, getFirst), (<>))

import Common.Maybes    (check)
import Common.Operators ((>$>))


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
mapHeadOrElse f def = maybe def f . headOpt

intercalate :: (Foldable f, Monoid m) => m -> f m -> m
intercalate incut = aux . toList where
  aux []       = mempty
  aux [a]      = a
  aux (x : xs) = x <> incut <> aux xs

mapFind :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
mapFind f = getFirst . foldMap (First . f)

average :: (Foldable f, Real a) => f a -> Maybe Rational
average = check notNull >$> \v -> toRational (sum v) / toRational (length v)
