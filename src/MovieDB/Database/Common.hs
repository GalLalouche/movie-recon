module MovieDB.Database.Common where

import Data.Text (Text)
import Control.Monad.Trans.Reader (ReaderT)


newtype DbPath = DbPath { path :: Text }
type DbCall a = ReaderT DbPath IO a
