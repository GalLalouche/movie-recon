{-# LANGUAGE DeriveFunctor #-}

module Common.TestCommon where

import Data.Foldable     (toList)
import Data.List         (sort)

import Control.Exception (SomeException(..), catch)

import Test.Tasty.HUnit  ((@?), (@?=))


newtype Box a = Box a deriving (Functor, Eq, Show)
instance Applicative Box where
  pure = Box
  (Box f) <*> (Box b) = pure $ f b
instance Monad Box where
  (Box b) >>= f = f b

assertThrows :: a -> IO ()
assertThrows a = checkThrows a @? "Expected an some exception to be thrown but none was thrown" where
  checkThrows :: a -> IO Bool
  checkThrows a = seq a (return False) `catch` throws where
    throws (SomeException _) = return True

-- Order-agnostic equality check
(*?=) :: (Foldable f1, Foldable f2, Ord a, Show a) => f1 a -> f2 a -> IO ()
a *?= b = sort (toList a) @?= sort (toList b)
