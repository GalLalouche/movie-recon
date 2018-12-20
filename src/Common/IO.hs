module Common.IO where

import Prelude hiding (catch)
import System.Directory (removeFile, doesFileExist)
import Control.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)


removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- Me fail English? That's unpossible!
createFileIfNotExists :: FilePath -> IO ()
createFileIfNotExists file = do
  exists <- doesFileExist file
  if exists then return () else writeFile file ""
