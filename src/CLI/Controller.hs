{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : CLI.Controller
Description : Controller for the ASCII Art Generator CLI
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

The Controller module for the ASCII Art Generator command line interface (CLI). This module 
contains the orchestration logic to handle user input, process images, and output the resulting ASCII art.

The main entry point is the 'runCli' function which handles the high level execution flow, 
catches exceptions, and handles errors.
-}
module CLI.Controller
  ( runCli
  ) where

import CLI.Parser
import Control.Exception
import Filters.Filter
import Loaders.Loader
import Options.Applicative
import Output.Output
import System.IO
import Types.AsciiImage
import Types.Exception
import Types.Image
import Utils.FileSystem

import Control.Monad (unless)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import System.Exit (exitFailure)

-- | 'supportedLoaders' is a list of supported image file loaders. Each loader is a tuple consisting of
-- a string that represents the file extension, and a function that takes a FilePath and returns a Loader.
supportedLoaders :: [(String, FilePath -> Loader)]
supportedLoaders = [("png", PngLoader), ("jpg", JpgLoader)]

-- | 'getLoader' takes a file extension as a string and returns the appropriate Loader function
-- from 'supportedLoaders', or Nothing if the file extension is not supported.
getLoader :: String -> Maybe (FilePath -> Loader)
getLoader ext = lookup ext supportedLoaders

-- | 'handleInput' takes an Options value and returns the image at the source file path specified in Options.
-- This function throws an ImageLoadException if the source file does not exist, is not recognized, or is not supported.
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

-- | 'handleOutput' takes an Options value and an AsciiImage, and outputs the AsciiImage to the locations specified in Options.
handleOutput :: Options -> AsciiImage -> IO ()
handleOutput options asciiImage = do
  let outputHandlers =
        [CliOutput | outputToConsole options] ++
        [FileOutput outputPath | Just outputPath <- [outputFile options]]
  mapM_ (`outputAsciiArt` asciiImage) outputHandlers

-- | 'handleAsciiArt' takes an Options value and an Image, applies the filters specified in Options to the Image, 
-- and converts the filtered image to an AsciiImage.
handleAsciiArt :: Options -> Image -> AsciiImage
handleAsciiArt options image =
  let asciiImage = toAsciiArt image
      imageFilters =
        [InvertFilter | invert options] ++
        [FlipFilter flipFilterAxis | Just flipFilterAxis <- [flipAxis options]] ++
        [ RotateFilter rotateFilterDegrees
        | Just rotateFilterDegrees <- [rotationDegrees options]
        ] ++
        [ BrightnessFilter brightnessFiltervalue
        | Just brightnessFiltervalue <- [brightnessValue options]
        ] ++
        [ ScaleFilter scaleFilterfactor
        | Just scaleFilterfactor <- [scalingFactor options]
        ]
   in foldl applyFilter asciiImage imageFilters

-- | 'handleException' is a function that takes a MyException value, outputs the error message to stderr, 
-- and exits the program with a failure status.
handleException :: MyException -> IO ()
handleException e = do
  case e of
    ImageDecodeException msg -> hPutStrLn stderr msg
    ImageLoadException msg -> hPutStrLn stderr msg
  exitFailure

-- | 'runAsciiArtGenerator' is the main function for the ASCII Art Generator logic. 
-- It parses command line options, handles input and output, and applies filters to images.
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

-- | 'runCli' is the main entry point for the ASCII Art Generator CLI. It executes the 
-- 'runAsciiArtGenerator' function and catches any exceptions that may be thrown, handling them 
-- gracefully by outputting the error to stderr and exiting with a failure status.
runCli :: IO ()
runCli = catch runAsciiArtGenerator handleException
