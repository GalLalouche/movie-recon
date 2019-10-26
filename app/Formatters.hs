{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}

module Formatters where

import           Data.Foldable           (toList)
import           Data.String.Interpolate (i)

import           MovieDB.Types           (Movie(..), pattern MovieId, Participation(..), ParticipationType(Actor), Person)
import qualified MovieDB.Types           as Types
import           OMDB                    (MovieScore(..), MovieScores, Source(IMDB, Metacritic, RottenTomatoes))
import qualified OMDB

import           Common.Foldables        (intercalate)


mkStringMovie :: Movie -> Maybe MovieScores -> String
mkStringMovie (Movie (MovieId id) name date) ms = let
    scoreString = maybe "No score" (toString . toList . OMDB._scores) ms
    toString ms = [i|(#{intercalate ", " $ fmap aux ms})|]
    aux (MovieScore source score) = let
        shortSource = case source of
          IMDB           -> "IMDB"
          RottenTomatoes -> "RT"
          Metacritic     -> "MC"
      in [i|#{score} #{shortSource}|]
  in [i|#{id}\t#{name}\t#{date}\t#{scoreString}|]

data FullMovieInfo = FullMovieInfo Movie [Participation] (Maybe MovieScores)
mkFullMovieInfoString :: FullMovieInfo -> String
mkFullMovieInfoString (FullMovieInfo m ps ms) = let
    movieString = mkStringMovie m ms
    participationStrings = map mkStringParticipation ps
  in unlines $ movieString : participationStrings where
  mkStringParticipation (Participation p _ pt) = let
      maybeRole = case pt of
        Actor -> ""
        e     -> [i| (#{e})|]
      name = Types._name (p :: Person)
    in [i|\t#{name}#{maybeRole}|]
