module Common.MaybeTUtils where

import           Common.Operators

import           Control.Monad             ((>=>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import           Data.List.NonEmpty        (NonEmpty((:|)))
import qualified Data.Maybe                as Maybe


-- Returns Nothing if the list is empty, or Just of a non-empty list
fromList :: Monad m => m [a] -> MaybeT m (NonEmpty a)
fromList = lift >=> cases .> return .> MaybeT where
  cases [] = Nothing
  cases (x : xs) = Just $ x :| xs

-- (Unsafely) peel away the MaybeT transformer.
fromJust :: Monad m => MaybeT m a -> m a
fromJust = (Maybe.fromJust <$>) . runMaybeT
