{-|
Module      : Output.Output
Description : Handles output of ASCII art to either a file or console
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module provides an abstraction over the output medium (file or console) for ASCII art.
-}
module Output.Output
  ( Output(..)
  , AsciiArtOutput(..)
  ) where

import System.IO
import Types.AsciiImage

-- | 'Output' data type represents possible output destinations.
data Output
  = FileOutput FilePath -- ^ 'FileOutput' constructor represents file output. It contains 'FilePath' to the output file.
  | CliOutput -- ^ 'CliOutput' constructor represents console output.

-- | 'AsciiArtOutput' typeclass provides a common interface for outputting ASCII art.
class AsciiArtOutput a
  -- | 'outputAsciiArt' is a function that takes an instance of a class implementing 'AsciiArtOutput' and an 'AsciiImage' 
  --   and performs IO operation to output the image.
  where
  outputAsciiArt :: a -> AsciiImage -> IO ()

-- | 'AsciiArtOutput' instance for 'Output' type.
instance AsciiArtOutput Output
  -- | For 'CliOutput', 'outputAsciiArt' will print the ASCII art to the console using 'putStrLn'.
                                                                                                   where
  outputAsciiArt CliOutput asciiImage = putStrLn (prettyPrint asciiImage)
  -- | For 'FileOutput', 'outputAsciiArt' will open the file at the specified path, write the ASCII art into the file, 
  --   and then close the file handle.
  outputAsciiArt (FileOutput outputPath) asciiImage = do
    handle <- openFile outputPath WriteMode
    hPutStr handle (prettyPrint asciiImage)
    hClose handle
