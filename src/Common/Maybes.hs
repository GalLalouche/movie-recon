module Common.Maybes where

orError :: String -> Maybe a -> a
orError _ (Just x) = x
orError msg Nothing = error msg
