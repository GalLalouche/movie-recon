module Common.ExceptTUtils where

import Data.Either.Combinators    (maybeToRight)

import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)

import Common.Operators
import Control.Monad              ((>=>))


toExcept :: Functor m => b -> MaybeT m a -> ExceptT b m a
toExcept def = ExceptT . fmap (maybeToRight def) . runMaybeT

handle :: Functor m => (Either e a -> c) -> ExceptT e m a -> m c
handle = (>$>) runExceptT

mhandle :: Monad m => (Either e a -> m c) -> ExceptT e m a -> m c
mhandle = (>=>) runExceptT

meither :: Monad m => (e -> m c) -> (a -> m c) -> ExceptT e m a -> m c
meither f g = mhandle aux where
  aux (Left x)  = f x
  aux (Right x) = g x
