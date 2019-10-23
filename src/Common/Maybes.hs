module Common.Maybes where


orError :: String -> Maybe a -> a
orError _ (Just x) = x
orError msg Nothing = error msg

check :: (a -> Bool) -> a -> Maybe a
check p a = if p a then Just a else Nothing

fcheck :: Functor f => (a -> f Bool) -> a -> f (Maybe a)
fcheck f a = (\b -> if b then Just a else Nothing) <$> f a
