{-# LANGUAGE LambdaCase #-}

module CLI.Parser (Options(..), optionsParser) where

import Filters.Filter
import Options.Applicative



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
          (\case
               "x" -> Right AxisX
               "y" -> Right AxisY
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