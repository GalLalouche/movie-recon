module Common.IO where

import Data.Time         (Day, getCurrentTime)
import Data.Time.Clock   (utctDay)

import Control.Exception (catch, throwIO)
import System.Directory  (doesFileExist, removeFile)
import System.IO.Error   (isDoesNotExistError)


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

getCurrentDate :: IO Day
getCurrentDate = utctDay <$> getCurrentTime
