module Common.Functors where

import Control.Arrow              ((&&&))


fproduct :: (Functor f) => (a -> b) -> f a -> f (a, b)
fproduct = fmap . (id &&&)
