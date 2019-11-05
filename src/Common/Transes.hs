{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Common.Transes(
  lift,
  transBindTop,
  (^=<<),
  (&=<<),
  (>>=^),
  (>>=&),
  transBindBot,
) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

import Common.Operators          ((.>), (>$>))


class (MonadTrans t, Monad tm) => RunnableMonadTrans t tm where
  run :: Monad m => t m a -> m (tm a)
  wrap :: Monad m => m (tm a) -> t m a

instance RunnableMonadTrans MaybeT Maybe where
  run = runMaybeT
  wrap = MaybeT

-- Lifts a monadic function to the bottom monad to the trans monad.
transBindTop :: (MonadTrans t, Monad m, Monad (t m)) => (a -> m b) -> t m a -> t m b
transBindTop f = (=<<) (lift . f)

-- Lifts a monadic function to the top monad to the trans monad.
transBindBot :: (RunnableMonadTrans t bot, Monad bot, Monad top) => (a -> bot b) -> t top a -> t top b
transBindBot f = (run >$> (=<<) f) .> wrap

(^=<<) :: (MonadTrans t, Monad m, Monad (t m)) => (a -> m b) -> t m a -> t m b
(^=<<) = transBindTop
infixr 1 ^=<< -- Same as =<<

(&=<<) :: (RunnableMonadTrans t bot, Monad bot, Monad top) => (a -> bot b) -> t top a -> t top b
(&=<<) = transBindBot
infixr 1 &=<< -- Same as =<<

(>>=^) :: (MonadTrans t, Monad m, Monad (t m)) => t m a -> (a -> m b) -> t m b
(>>=^) = flip (^=<<)
infixl 1 >>=^ -- Same as >>=

(>>=&) :: (RunnableMonadTrans t bot, Monad bot, Monad top) => t top a -> (a -> bot b) -> t top b
(>>=&) = flip (&=<<)
infixl 1 >>=& -- Same as >>=
