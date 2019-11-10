module Common.Arrows where

import Common.Applicatives as Applicatives
import Control.Arrow       ((&&&))


uncurryA2 :: Applicative f => (a -> b -> c) -> (x -> f a) -> (x -> f b) -> x -> f c
uncurryA2 ctor f g = Applicatives.uncurryLift2 ctor . (f &&& g)
