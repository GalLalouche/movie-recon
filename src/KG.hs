module KG where

import Control.Monad.Reader
import Recon
import Data.Text

readApiKey :: IO ApiKey
readApiKey = (ApiKey . strip . pack) <$> readFile "keys/kg.txt"

newtype ApiKey = ApiKey { key :: Text } deriving Show
reconciler :: Reader ApiKey Reconciler
reconciler = undefined

