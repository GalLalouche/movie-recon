{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuasiQuotes           #-}

module Main.Action.Internal.Format(
  mkStringMovie,
  FullMovieInfo(..),
  mkFullMovieInfoString,
) where

import           Prelude                       hiding (unlines)

import           Data.Foldable                 (toList)
import qualified Data.Set                      as Set (map)
import           Data.Text                     (Text, unlines)
import           Data.Vector                   (Vector)
import           Text.InterpolatedString.Perl6 (qq)

import           MovieDB.Types                 (Movie(Movie), pattern MovieId, Participation(Participation), ParticipationType(Actor), Person(_name))
import           OMDB                          (MovieScore(MovieScore), MovieScores(_scores), Source(IMDB, Metacritic, RottenTomatoes))

import           Common.Foldables              (intercalate)


tab = "\t"

mkStringMovie :: Movie -> Maybe MovieScores -> Text
mkStringMovie (Movie (MovieId id) name date) ms = let
    scoreString = maybe "No score" (toString . _scores) ms
    toString ms = let commaSeparatedScores = intercalate ", " $ Set.map aux ms in [qq|({commaSeparatedScores})|]
    aux (MovieScore source score) = let
        shortSource = case source of
          IMDB           -> "IMDB"
          RottenTomatoes -> "RT"
          Metacritic     -> "MC"
      in [qq|$score $shortSource|]
  in [qq|$id$tab$name$tab$date$tab$scoreString|]

data FullMovieInfo = FullMovieInfo
  { movie          :: Movie
  , participations :: Vector Participation
  , scores         :: Maybe MovieScores
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
    in [qq|$tab{_name p}$maybeRole|]
