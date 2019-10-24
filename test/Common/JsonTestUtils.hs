module Common.JsonTestUtils where

import Data.ByteString.Lazy (readFile)
import Prelude              hiding (readFile)

import Common.JsonUtils     (ObjectParser, decodeUnsafe, fromSuccess, parseObject)


parseJson :: ObjectParser a -> FilePath -> IO a
parseJson parser fileName = do
   json <- readFile $ "test/resources/" ++ fileName ++ ".json"
   return $ fromSuccess $ parseObject parser (decodeUnsafe json)
