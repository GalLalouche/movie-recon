{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Common.Operators where

import Data.Function   ((&))

import Control.Monad   ((>=>))

import Common.Functors (fproduct)


(|>) :: a -> (a -> b) -> b
(|>) = (&) -- I think |> is more expressive than &
(|>>) :: a -> (b -> a -> c) -> b -> c
-- (|>>) = flip flip, but nah...
(a |>> f) b = f b a
(|>>>) :: a -> (b -> c -> a -> d) -> b -> c -> d
(x |>>> f) b c = f b c x
infixl 1 |>, |>>, |>>> -- Same priority as &

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
(..>) :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
(..>) f g a = g . f a
(...>) :: (a -> b -> c -> d) -> (d -> e) -> (a -> b -> c -> e)
(...>) f g a b = g . f a b
infixr 9 .>, ..>, ...> -- same priority as .

(-$>) :: Functor f => (a -> b) -> f a -> f (a, b)
(-$>) = fproduct
(<$-) :: Functor f => f a -> (a -> b) -> f (a, b)
(<$-) = flip (-$>)

-- fmap equivalent of <**>
-- In lens and base >= 4.11, this is defined as <&> but I like the symmetry w.r.t. <**>
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
infixl 1 <$$> -- Same priority as <&>

-- fmap equivalent of >=> and <=<
(>$>) :: Functor m => (a -> m b) -> (b -> c) -> (a -> m c)
(f >$> g) a = f a <$$> g

(<$<) :: Functor m => (b -> c) -> (a -> m b) -> (a -> m c)
(<$<) = flip (>$>)

-- >=> and <=< for two parameters, or ..> for monads
(>==>) :: Monad m => (a -> b -> m c) -> (c -> m d) -> a -> b -> m d
(>==>) f g a = f a >=> g

(<==<) :: Monad m => (c -> m d) -> (a -> b -> m c) -> a -> b -> m d
(<==<) = flip (>==>)

-- fmap equivalent of the above
(>$$>) :: Functor m => (a -> b -> m c) -> (c -> d) -> a -> b -> m d
(>$$>) f g a = f a >$> g

(<$$<) :: Functor m => (c -> d) -> (a -> b -> m c) -> a -> b -> m d
(<$$<) = flip (>$$>)

infixr 1 -$>, >$>, >$$>, >==> -- same priority as >=>
infixr 1 <$-, <$<, <$$<, <==< -- same priority as <=<

-- Allows for the following syntax:
-- > ctor <$> monadic_a <*$> non_monadic_b <*$> non_monadic_c
-- As opposed to:
-- > ctor <$> monadic_a <*> return non_monadic_b <*> return non_monadic_c
-- In lens and other places, this is named (??), but I like the symmetry of <*$> better.
(<*$>) :: Applicative m => m (a -> b) -> a -> m b
f <*$> x = f <*> pure x
-- Allows for the following syntax:
-- > ctor <$*> non_monadic_a <*> monadic_b
-- As opposed to:
-- > ctor <$> return non_monadic_a <*> monadic_b
(<$*>) :: Applicative m => (a -> b) -> a -> m b
(<$*>) f x = f <$> pure x
infixl 4 <*$>, <$*> -- same priority as <*>

-- Operator taken from here: https://stackoverflow.com/a/51097392/736508
(.:) :: (midResult -> finalResult) -> (a -> b -> midResult) -> (a -> b -> finalResult)
(.:) f g x y = f $ g x y -- (.).(.) also works, see also: https://wiki.haskell.org/Pointfree#Dot
(..:) :: (midResult -> finalResult) -> (a -> b -> c -> midResult) -> (a -> b -> c -> finalResult)
(..:) f g x y z = f $ g x y z -- (.:).(.) also works
infixr 8 .:, ..: -- one below (.), so f . g .: h can be used without parens
