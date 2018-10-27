module Common.Operators where

import Control.Monad ((>=>), (<=<))

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 4 |>

(|>>) :: a -> (b -> a -> c) -> b -> c
-- (|>>) = flip flip nah...
(a |>> f) b = f b a
infixl 4 |>>

(|>>>) :: a -> (b -> c -> a -> d) -> b -> c -> d
(x |>>> f) b c = f b c x
infixl 4 |>>>

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 4 .>

(..>) :: (a -> b -> c) -> (c -> d) -> (a -> b -> d)
(..>) f g a = g . f a
infixl 4 ..>

(...>) :: (a -> b -> c -> d) -> (d -> e) -> (a -> b -> c -> e)
(...>) f g a b = g . f a b
infixl 4 ...>

fproduct :: Functor f => (a -> b) -> f a -> f (a, b)
fproduct f = fmap (\x -> (x, f x))

(-$>) :: Functor f => (a -> b) -> f a -> f (a, b)
(-$>) = fproduct
infixl 4 -$>

(<$-) :: Functor f => f a -> (a -> b) -> f (a, b)
(<$-) = flip (-$>)
infixl 4 <$-

-- fmap equivalent of <**>
(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

infixl 4 <$$> -- Same priority as <**>

-- fmap equivalent of >=> and <=<
(>$>) :: Functor m => (a -> m b) -> (b -> c) -> (a -> m c)
(f >$> g) a = f a <$$> g
infixr 1 >$> -- same priority as >=>

(<$<) :: Functor m => (b -> c) -> (a -> m b) -> (a -> m c)
(<$<) = flip (>$>)

infixr 1 <$< -- same priority as <=<

-- >=> and <=< for two parameters, or ..> for monads
(>==>) :: Monad m => (a -> b -> m c) -> (c -> m d) -> a -> b -> m d
(>==>) f g a = f a >=> g
infixr 1 >==> -- same priority as >=>

(<==<) :: Monad m => (c -> m d) -> (a -> b -> m c) -> a -> b -> m d
(<==<) = flip (>==>)
infixr 1 <==< -- same priority as >=>
