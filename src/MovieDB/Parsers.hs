{-# LANGUAGE OverloadedStrings #-}

module MovieDB.Parsers where

import Data.Aeson ((.:))
import MovieDB.Types
import Common.JsonUtils
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J

import Common.Foldables (headUnsafe)
import Common.Operators
import Control.Monad ((<=<), (>=>))


parseSearchPerson :: J.Value -> PersonId
parseSearchPerson = fromSuccess . J.parse parser where
  parser :: J.Value -> J.Parser PersonId
  parser = let
      results = asObject >=> objects "results"
      firstResult = headUnsafe <$< results
      id = firstResult >=> int "id"
    in PersonId <$< id

