{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Config(Config(..), parseConfig) where

import System.Console.CmdArgs          (Data, Typeable, argPos, cmdArgs, def, help, modes, typ, typFile, (&=))
import System.Console.CmdArgs.Implicit


data Config =
    UpdateSeen
    | GetUnseen {verbose :: Bool}
    | UpdateIndex
    | AddPerson {url :: String}
    deriving (Show, Data, Typeable, Eq)

updateSeen = UpdateSeen &= help (
    "Reads a list of seen movie IDs to update seen movies from stdin.\n" ++
    "Every line should start with an I or S (Ignored or Seen), followed by an ID, " ++
    "and optionally more text separated by <TAB>. Example line:\n" ++
    "\"S299536<TAB>Avengers: Infinity War<TAB>2018-04-27\""
  )
getUnseen = GetUnseen { verbose = def &=
    help "If true, also prints the followed cast and crew for the film"
    } &= help "Return all unseen movies, their release date, and their IDs."
updateIndex = UpdateIndex &= help "Updates the index of movies for all followed persons."
addPerson = AddPerson {url = def &= argPos 0
    &= typ "MovieDB URL, e.g., https://www.themoviedb.org/person/17697-john-krasinski"
    } &= help "Adds a followed person"

parseConfig :: IO Config
parseConfig = cmdArgs $ modes [updateSeen, getUnseen, updateIndex, addPerson]
