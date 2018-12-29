module Common.MonadPluses where

import Common.Operators

import Data.Maybe    (isJust, fromJust)

import Control.Monad (MonadPlus, mfilter)

catMaybes :: MonadPlus m => m (Maybe a) -> m a
catMaybes = fmap fromJust . mfilter isJust

fmapMaybes :: MonadPlus m => (a -> Maybe b) -> m a -> m b
fmapMaybes = catMaybes .: fmap
