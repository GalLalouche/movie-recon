{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main.Action.Internal.Format(
  mkStringMovie,
  FullMovieInfo(..),
  mkFullMovieInfoString,
) where

import           Prelude                       hiding (unlines)

import           Data.Foldable                 (toList)
import           Data.Text                     (Text, unlines)
import           Data.Vector                   (Vector)
import           Text.InterpolatedString.Perl6 (qq)

import           MovieDB.Types                 (Movie(Movie), pattern MovieId, Participation(Participation), ParticipationType(Actor), Person)
import qualified MovieDB.Types                 as Types
import           OMDB                          (MovieScore(MovieScore), MovieScores, Source(IMDB, Metacritic, RottenTomatoes))
import qualified OMDB

import           Common.Foldables              (intercalate)


tab = "\t"

mkStringMovie :: Movie -> Maybe MovieScores -> Text
mkStringMovie (Movie (MovieId id) name date) ms = let
    scoreString = maybe "No score" (toString . toList . OMDB._scores) ms
    toString ms = [qq|({intercalate ", " $ fmap aux ms})|]
    aux (MovieScore source score) = let
        shortSource = case source of
          IMDB           -> "IMDB"
          RottenTomatoes -> "RT"
          Metacritic     -> "MC"
      in [qq|$score $shortSource|]
  in [qq|$id$tab$name$tab$date$tab$scoreString|]

data FullMovieInfo = FullMovieInfo 
  { movie :: Movie
  , participations :: Vector Participation
  , scores :: Maybe MovieScores
  }

mkFullMovieInfoString :: FullMovieInfo -> Text
mkFullMovieInfoString (FullMovieInfo m ps ms) = let
    movieString = mkStringMovie m ms
    participationStrings = fmap mkStringParticipation ps
  in unlines $ movieString : toList participationStrings where
  mkStringParticipation (Participation p _ pt) = let
      maybeRole = case pt of
        Actor -> ""
        e     -> [qq| ($e)|]
      name = Types._name (p :: Person)
    in [qq|$tab$name$maybeRole|]
