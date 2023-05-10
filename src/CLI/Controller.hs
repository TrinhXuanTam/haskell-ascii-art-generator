{-# LANGUAGE OverloadedStrings #-}

module CLI.Controller (runCli) where

import Control.Exception
import Filters.Filter
import Loaders.Loader
import Output.Output
import System.IO
import Types.AsciiImage
import Types.Exception
import Types.Image
import Utils.FileSystem
import CLI.Parser
import Options.Applicative

import Control.Monad (unless)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import System.Exit (exitFailure)


-- | List of supported loaders
supportedLoaders :: [(String, FilePath -> Loader)]
supportedLoaders = [("png", PngLoader), ("jpg", JpgLoader)]

-- | Function to get the appropriate loader based on the file extension
getLoader :: String -> Maybe (FilePath -> Loader)
getLoader ext = lookup ext supportedLoaders

handleInput :: Options -> IO Image
handleInput options = do
  let file = sourcePath options
  fileDoesExist <- fileExists file
  unless fileDoesExist $
    throw $ ImageLoadException "The source file does not exist!"
  let fileExtension = getFileExtension file
  unless (isJust fileExtension) $
    throw $ ImageLoadException "The source file format not recognized!"
  image <-
    case getLoader (fromJust fileExtension) of
      Just loader -> Just <$> loadImage (loader file)
      Nothing -> return Nothing
  unless (isJust image) $
    throw $ ImageLoadException "The source file format is not supported!"
  return $ fromJust image

handleOutput :: Options -> AsciiImage -> IO ()
handleOutput options asciiImage = do
  let outputHandlers =
        [CliOutput | outputToConsole options] ++
        [FileOutput outputPath | Just outputPath <- [outputFile options]]
  mapM_ (`outputAsciiArt` asciiImage) outputHandlers

-- call outputAsciiArt on each element of outputhandlers
handleAsciiArt :: Options -> Image -> AsciiImage
handleAsciiArt options image =
  let asciiImage = toAsciiArt image
      imageFilters =
        [InvertFilter | invert options] ++
        [FlipFilter flipFilterAxis | Just flipFilterAxis <- [flipAxis options]] ++
        [RotateFilter rotateFilterDegrees | Just rotateFilterDegrees <- [rotationDegrees options]] ++
        [BrightnessFilter brightnessFiltervalue | Just brightnessFiltervalue <- [brightnessValue options]] ++
        [ScaleFilter scaleFilterfactor | Just scaleFilterfactor <- [scalingFactor options]]
   in foldl applyFilter asciiImage imageFilters

-- | Handle exceptions related to the application logic and print error messages to stderr
handleException :: MyException -> IO ()
handleException e = do
  case e of
    ImageDecodeException msg -> hPutStrLn stderr msg
    ImageLoadException msg -> hPutStrLn stderr msg
  exitFailure

runAsciiArtGenerator :: IO ()
runAsciiArtGenerator = do
  options <-
    execParser $
    info
      (optionsParser <**> helper)
      (fullDesc <>
       progDesc
         ("A command-line utility that takes an image from a provided input path, converts the image into ASCII art, and optionally applies text filters. The supported image formats are: " ++
          intercalate ", " (map fst supportedLoaders) ++ ".") <>
       header "ASCII Art Generator")
  image <- handleInput options
  let asciiImage = handleAsciiArt options image
  handleOutput options asciiImage

runCli :: IO ()
runCli = catch runAsciiArtGenerator handleException