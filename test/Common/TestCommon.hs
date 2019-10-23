{-# LANGUAGE DeriveFunctor       #-}

module Common.TestCommon where


newtype Box a = Box a deriving (Functor, Eq, Show)
instance Applicative Box where
  pure = Box
  (Box f) <*> (Box b) = pure $ f b
instance Monad Box where
  (Box b) >>= f = f b
