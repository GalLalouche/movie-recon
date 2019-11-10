module Common.Applicatives where

import Control.Applicative (liftA2)


uncurryLift2 :: Applicative f => (a -> b -> c) -> (f a, f b) -> f c
uncurryLift2 = uncurry . liftA2
