module KG where

import Control.Monad.Reader (Reader)
import Recon (Reconciler)
import Data.Text (Text, pack, strip)

readApiKey :: IO ApiKey
readApiKey = (ApiKey . strip . pack) <$> readFile "keys/kg.txt"

newtype ApiKey = ApiKey { key :: Text } deriving Show
reconciler :: Reader ApiKey Reconciler
reconciler = undefined

