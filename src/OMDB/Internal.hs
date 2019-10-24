{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module OMDB.Internal where

import Common.JsonUtils        (ObjectParser, str, withObjects)
import Data.Char               (isDigit)
import Data.String.Interpolate
import Data.Text               (Text, index, unpack)
import Data.Vector             (Vector)


data Source = IMDB | RottenTomatoes | Metacritic deriving (Eq, Show, Ord, Read) -- Extending Ord for saving in Sets
readSource :: Text -> Source
readSource t
  | t == "Internet Movie Database" = IMDB
  | t == "Rotten Tomatoes" = RottenTomatoes
  | t == "Metacritic" = Metacritic
  | otherwise = error [i|Unrecongnized source <#{t}>|]

data MovieScore = MovieScore
  { _source :: Source
  , _score  :: Int
  } deriving (Eq, Show, Ord) -- Extending Ord for saving in Sets

parse :: ObjectParser (Vector MovieScore)
parse = withObjects "Ratings" aux where
  aux :: ObjectParser MovieScore
  aux = do
    source <- readSource <$> str "Source"
    score <- parseScore source <$> str "Value"
    return $ MovieScore source score
  parseScore :: Source -> Text -> Int
  parseScore IMDB           = \t -> read [index t 0, index t 2] -- Format is x.y/10.0
  parseScore RottenTomatoes = readDigits -- Format is <number>%
  parseScore Metacritic     = readDigits -- Format is <number>/100
  readDigits = read . takeWhile isDigit . unpack
