{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Parsers where

import MovieDB.Types    (Movie(..), ParticipationType(..), Person(..), PersonId(..))

import Data.Foldable    (toList)
import Data.Semigroup   ((<>))
import Data.Text        (pack)

import Data.Aeson       (Object, Value)
import Data.Aeson.Types (Parser, parse)

import Common.JsonUtils (asObject, fromSuccess, int, objects, str)
import Common.Operators


parseMovieCredits :: Value -> Parser [(Person, ParticipationType)]
parseMovieCredits o = do
    object <- o |> asObject
    cast <- object |> objects "cast"
    crew <- object |> objects "crew"
    parsedCast <- traverse parseCast cast
    parsedCrew <- traverse parseCrew crew
    return $ toList $ parsedCast <> parsedCrew where
      getName = str "name"
      getId = int "id" >$> (show .> pack .> PersonId)
      withRole role o = do
          name <- o |> getName
          id <- o |> getId
          return (Person id name, role)
      parseCast :: Object -> Parser (Person, ParticipationType)
      parseCast = withRole Actor
      parseCrew :: Object -> Parser (Person, ParticipationType)
      parseCrew o = parseCrewRole o >>= flip withRole o
      parseCrewRole :: Object -> Parser ParticipationType
      parseCrewRole o = do
        job <- o |> str "job"
        return $ case job of
          "Director"   -> Director
          "Screenplay" -> Writer
