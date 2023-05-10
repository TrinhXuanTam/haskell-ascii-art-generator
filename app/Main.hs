{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Options.Applicative
import Utils.FileSystem
import Utils.Control
import Loaders.Loader
import Loaders.PngLoader
import Loaders.JpgLoader
import Loaders.Image
import Types.Exception

import System.Exit (exitFailure)
import Data.Maybe (isNothing, fromJust)
import Control.Exception (catch)
import System.IO

-- A data type to hold the command line options
data Options = Options
  { sourcePath :: FilePath
  }


-- A parser for the command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "src"
     <> metavar "FILE"
     <> help "The path to the source image file" )


-- A function to load an image from file based on its file format
loadImageBySuffixAndPath :: String -> FilePath -> IO (Maybe Image)
loadImageBySuffixAndPath "png" filePath = Just <$> loadImage (PngLoader (filePath))
loadImageBySuffixAndPath "jpg" filePath = Just <$> loadImage (JpgLoader (filePath)) 
loadImageBySuffixAndPath _ _ = return Nothing


-- | Handle exceptions related to the application logic and print error messages to stderr
handleException :: MyException -> IO ()
handleException e = do
    case e of
        ImageDecodeException msg -> hPutStrLn stderr $ msg
    exitFailure


runAsciiArtGenerator :: IO ()
runAsciiArtGenerator = do
  options <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "A command-line utility that takes an image from a provided input path, converts the image into ASCII art, and optionally applies filters."
   <> header "ASCII Art Generator" )
  
  let file = sourcePath options
  fileDoesExist <- fileExists file
  conditionalExitWithFailure (not fileDoesExist) "The source file does not exist!"

  let fileSuffix = getFileSuffix file 
  conditionalExitWithFailure (isNothing fileSuffix) "The source file format not recognized!"

  image <- loadImageBySuffixAndPath (fromJust fileSuffix) file
  conditionalExitWithFailure (isNothing image) "The source file format is not supported!"
  
  putStrLn $ show image



-- Driver function
main :: IO ()
main = catch runAsciiArtGenerator handleException