module Common.MaybeTs where

import           Data.Foldable             (toList)
import           Data.List.NonEmpty        (NonEmpty((:|)))
import qualified Data.Maybe                as Maybe

import           Control.Monad             ((>=>))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import           Common.Operators


-- Returns Nothing if the foldable is empty, or Just of a non-empty list
fromFoldable :: (Monad m, Foldable f) => m (f a) -> MaybeT m (NonEmpty a)
fromFoldable = lift >=> toList .> cases .> return .> MaybeT where
  cases []       = Nothing
  cases (x : xs) = Just $ x :| xs

-- (Unsafely) peel away the MaybeT transformer.
fromJust :: Monad m => MaybeT m a -> m a
fromJust = (Maybe.fromJust <$>) . runMaybeT

isJust :: Functor m => MaybeT m a -> m Bool
isJust = Maybe.isJust <$< runMaybeT
