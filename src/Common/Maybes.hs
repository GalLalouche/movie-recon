module Common.Maybes where

import Data.Maybe        (fromMaybe, maybe)
import Data.Text         (Text)

import Common.Assertions (textError)


orError :: Text -> Maybe a -> a
orError _ (Just x)  = x
orError msg Nothing = textError msg

check :: (a -> Bool) -> a -> Maybe a
check p a = if p a then Just a else Nothing

fcheck :: Functor f => (a -> f Bool) -> a -> f (Maybe a)
fcheck f a = (\b -> if b then Just a else Nothing) <$> f a

mapIfOrNothing :: (a -> Bool) -> (a -> b) -> a -> Maybe b
mapIfOrNothing p f = fmap f . check p

orMempty :: Monoid a => Maybe a -> a
orMempty = fromMaybe mempty

mapMonoid :: Monoid b => (a -> b) -> Maybe a -> b
mapMonoid = maybe mempty
