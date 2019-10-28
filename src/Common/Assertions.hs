module Common.Assertions where

import Data.Text (Text, unpack)


textError :: Text -> a
textError = error . unpack

assertMsg :: Bool -> Text -> a -> a
assertMsg b msg a = if b then a else textError msg
