module Common.Functors where

fproduct :: (Functor f) => (a -> b) -> f a -> f (a, b)
fproduct f = fmap (\a -> (a, f a))

