{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Parsers where

import Common.Foldables (headUnsafe)
import Common.Operators
import Common.JsonUtils (asObject, objects, int, fromSuccess)

import Data.Text (pack)
import Data.Aeson ((.:))
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J

import Control.Monad ((<=<), (>=>))

import MovieDB.Types (PersonId(..))


parseSearchPerson :: J.Value -> PersonId
parseSearchPerson = fromSuccess . J.parse parser where
  parser :: J.Value -> J.Parser PersonId
  parser = let
      results = asObject >=> objects "results"
      firstResult = headUnsafe <$< results
      id = firstResult >=> int "id" >$> (pack .show)
    in PersonId <$< id

