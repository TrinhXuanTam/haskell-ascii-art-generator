{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Exception
import Filters.Filter
import Loaders.Loader
import Options.Applicative
import Output.Output
import System.IO
import Types.AsciiImage
import Types.Exception
import Types.Image
import Utils.Control
import Utils.FileSystem

import Control.Monad (guard, unless)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, isNothing)
import System.Exit (exitFailure)

-- A data type to hold the command line options
data Options =
  Options
    { sourcePath :: FilePath
    , outputToConsole :: Bool
    , outputFile :: Maybe FilePath
    , flipAxis :: Maybe Axis
    , rotationDegrees :: Maybe Int
    , scalingFactor :: Maybe Double
    , brightnessValue :: Maybe Int
    , invert :: Bool
    }

-- A parser for the command line options
optionsParser :: Parser Options
optionsParser =
  Options <$>
  strOption
    (long "src" <> metavar "FILE" <> help "The path to the source image file") <*>
  switch (long "output-console" <> help "Output ASCII art to console") <*>
  optional
    (strOption
       (long "output-file" <>
        metavar "FILE" <> help "The path to the output file")) <*>
  optional
    (option
       (eitherReader
          (\s ->
             case s of
               "x" -> Right (AxisX)
               "y" -> Right (AxisY)
               _ -> Left "Invalid axis value. Must be 'x' or 'y'."))
       (long "flip" <>
        metavar "AXIS" <> help "Flip the image along the X or Y axis")) <*>
  optional
    (option
       auto
       (long "rotate" <>
        metavar "DEGREES" <> help "Rotate the image by the specified degrees")) <*>
  optional
    (option
       auto
       (long "scale" <>
        metavar "FACTOR" <> help "Scale the image by the specified factor")) <*>
  optional
    (option
       auto
       (long "brightness" <>
        metavar "VALUE" <> help "Adjust the brightness of the image")) <*>
  switch (long "invert" <> help "Invert the colors of the image")

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
        [FlipFilter axis | Just axis <- [flipAxis options]] ++
        [RotateFilter degrees | Just degrees <- [rotationDegrees options]] ++
        [BrightnessFilter value | Just value <- [brightnessValue options]] ++
        [ScaleFilter factor | Just factor <- [scalingFactor options]]
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

-- Driver function
main :: IO ()
main = catch runAsciiArtGenerator handleException
