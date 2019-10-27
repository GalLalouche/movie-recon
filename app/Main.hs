{-# LANGUAGE OverloadedStrings #-}

import           MovieDB.Database           (DbCall, DbPath(..), runDbCall)

import           Control.Monad.Trans.Reader (runReaderT)

import qualified Actions
import qualified Config


dbPath = DbPath "db.sqlite"

withDb :: DbCall a -> IO a
withDb = flip runDbCall dbPath

withBoth :: Actions.APIAndDB -> IO ()
withBoth = flip runReaderT dbPath

main = do
  args <- Config.parseConfig
  case args of
    Config.Init                -> withDb $ Actions.initDatabases
    (Config.GetUnseen verbose) -> withDb $ Actions.printUnseenMovies verbose
    Config.UpdateSeen          -> withDb Actions.parseSeenMovies
    Config.UpdateIndex         -> withBoth Actions.updateMoviesForAllFollowedPersons
    Config.UpdateScores        -> withBoth Actions.updateScores
    (Config.AddPerson url)     -> withBoth $ Actions.addFollowedPerson url
