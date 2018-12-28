{-# LANGUAGE LambdaCase #-}

module Common.MaybeTUtils where

import Common.Operators

import Control.Monad             ((>=>))
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List.NonEmpty        (NonEmpty(..))
import qualified Data.Maybe as Maybe

-- A wrapper for MaybeT
just :: Monad m => m a -> MaybeT m a
just = MaybeT . (return <$>)

-- Returns Nothing if the list is empty, or Just of a non-empty list
fromList :: Monad m => m [a] -> MaybeT m (NonEmpty a)
fromList = just >=> cases .> return .> MaybeT where
  cases = \case
    []       -> Nothing
    (x : xs) -> return $ x :| xs

-- (Unsafely) peel away the MaybeT transformer.
fromJust :: Monad m => MaybeT m a -> m a
fromJust = (Maybe.fromJust <$>) . runMaybeT
