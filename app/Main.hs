{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Trans.Reader (runReaderT)

import           MovieDB.Database           (DbCall, DbPath(DbPath), runDbCall)

import           GHC.IO.Encoding            (setLocaleEncoding, utf8)

import qualified Main.Action                as Action
import qualified Main.Config                as Config


dbPath = DbPath "db.sqlite"

withDb :: DbCall a -> IO a
withDb = flip runDbCall dbPath

withBoth :: Action.JoinedAction -> IO ()
withBoth = flip runReaderT dbPath

main = do
  setLocaleEncoding utf8
  args <- Config.parseConfig
  case args of
    Config.Init                         -> withDb Action.initDatabases
    (Config.GetUnseen verbose)          -> withDb $ Action.printUnseenMovies verbose
    Config.UpdateSeen                   -> withDb Action.parseSeenMovies
    Config.UpdateIndex                  -> withBoth Action.updateMoviesForAllFollowedPersons
    Config.UpdateScores                 -> withBoth Action.updateScores
    (Config.AddPerson url ignoreActing) -> withBoth $ Action.addFollowedPerson url ignoreActing
