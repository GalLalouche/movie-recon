{-# LANGUAGE DuplicateRecordFields #-}

module Entity where
import Data.Text

newtype ReconId = ReconId { unreconId :: Text }
newtype EntityName = EntityName { name :: Text }
data PersonType = Actor | Director | Writer
data Score = Score { source :: Text, score :: Double}
data Movie = Movie {
    director :: Person
  , cast :: [Person]
  , scores :: [Score]
}
type Movies = [Movie]
data Person = Person {
    name :: Text
  , personType :: PersonType
}
