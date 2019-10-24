{-# LANGUAGE OverloadedStrings     #-}

import           MovieDB.Database.Common    (DbPath(..))

import           Control.Monad.Trans.Reader (ReaderT, runReaderT)

import           Common.Operators

import qualified Actions
import qualified Config


dbPath = DbPath "db.sqlite"

withDbPath :: Monad m => ReaderT DbPath m a -> m a
withDbPath = flip runReaderT dbPath

withBoth :: Actions.APIAndDB -> IO ()
withBoth = flip runReaderT dbPath


main = do
  args <- Config.parseConfig
  case args of
    (Config.GetUnseen verbose) -> withDbPath $ Actions.getFormattedUnseenMovies verbose
    Config.UpdateSeen          -> withDbPath Actions.parseSeenMovies
    Config.UpdateIndex         -> withBoth Actions.updateMoviesForAllFollowedPersons
    (Config.AddPerson url)     -> withBoth $ Actions.addFollowedPerson url
