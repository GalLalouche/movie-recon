module Common.Assertions where

assertMsg :: Bool -> String -> a -> a
assertMsg b msg a = if b then a else error msg
