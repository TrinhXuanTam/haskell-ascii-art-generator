{-|
Module      : Utils.FileSystem
Description : Provides utility functions related to file system operations
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module provides utility functions related to file system operations.
It includes a function to get the file extension from a file path and a function to check if a file exists.
-}
module Utils.FileSystem
  ( getFileExtension
  , fileExists
  ) where

import System.Directory (doesFileExist)

-- | 'getFileExtension' is a function that takes a file path and returns the file extension.
--   The file extension is defined as the string after the last dot in the file path.
--   If there is no dot in the file path, or if the file path ends with a dot, the function returns 'Nothing'.
getFileExtension :: FilePath -> Maybe String
getFileExtension filePath =
  let ext = reverse $ takeWhile (/= '.') $ reverse filePath
   in if ext == filePath || null ext
        then Nothing
        else Just ext

-- | 'fileExists' is a function that takes a file path and checks if a file exists at that path.
--   The function returns 'True' if the file exists and 'False' otherwise.
fileExists :: FilePath -> IO Bool
fileExists = doesFileExist
