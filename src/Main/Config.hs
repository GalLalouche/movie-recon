{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main.Config(Config(..), parseConfig) where

import Data.Text              (Text)

import System.Console.CmdArgs (Data, Default(..), Typeable, argPos, cmdArgs, def, help, modes, name, typ, (&=))


instance Default Text where def = ""

data Config =
      Init
    | UpdateSeen
    | GetUnseen {verbose :: Bool}
    | UpdateIndex
    | UpdateScores
    | AddPerson {url :: Text, noActing :: Bool}
    deriving (Show, Data, Typeable, Eq)

initDatabases = Init &= help "Initializes all databases"
updateSeen = UpdateSeen &= help (
    "Reads a list of seen movie IDs to update seen movies from stdin.\n" ++
    "Every line should start with an I, S, or L (Ignored, Seen, or Low scores), followed by an ID, " ++
    "and optionally more text separated by <TAB>. Example line:\n" ++
    "\"S299536<TAB>Avengers: Infinity War<TAB>2018-04-27\""
  )
getUnseen = GetUnseen { verbose = def &=
    help "If true, also prints the followed cast and crew for the film"
    } &= help "Return all unseen movies, their release date, and their IDs."
updateIndex = UpdateIndex &= help "Updates the index of movies for all followed persons."
updateScores = UpdateScores &= help "Updates the scores of movies in the index"
addPerson = AddPerson {
    url = def &= argPos 0 &= typ "MovieDB URL, e.g., https://www.themoviedb.org/person/17697-john-krasinski"
  , noActing = def &= name "na" &= help "If true, acting roles for this person would be ignored (e.g., director cameos)"
  } &= help "Adds a followed person"

parseConfig :: IO Config
parseConfig = cmdArgs $ modes [initDatabases, updateSeen, getUnseen, updateIndex, updateScores, addPerson]
