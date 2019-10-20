module Common.Traversables where

traverseFproduct :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t (a, b))
traverseFproduct f = traverse (\a -> (,) a <$> f a)
