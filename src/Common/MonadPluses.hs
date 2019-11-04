module Common.MonadPluses where

import Control.Monad    (MonadPlus, mfilter)
import Data.Maybe       (fromJust, isJust)

import Common.Maybes    (fcheck)
import Common.Operators ((.:), (<$$<))


catMaybes :: MonadPlus m => m (Maybe a) -> m a
catMaybes = fmap fromJust . mfilter isJust

mapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mapMaybe = catMaybes .: fmap

traverseFilter :: (MonadPlus m, Traversable m, Applicative f) => (a -> f Bool) -> m a -> f (m a)
traverseFilter = catMaybes <$$< traverse . fcheck
