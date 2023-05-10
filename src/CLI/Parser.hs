{-# LANGUAGE LambdaCase #-}

{-|
Module      : CLI.Parser
Description : Parser for the ASCII Art Generator CLI
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

The Parser module for the ASCII Art Generator command line interface (CLI). This module 
parses and validates the command line arguments, providing a structured 'Options' data type 
for the rest of the application to use.

The main entry point is the 'optionsParser' function which handles the parsing of command line arguments.
-}
module CLI.Parser
  ( Options(..)
  , optionsParser
  ) where

import Filters.Filter
import Options.Applicative

-- | A data type to hold the command line options. Each field corresponds to a possible command line argument.
data Options =
  Options
    { sourcePath :: FilePath -- ^ The path to the source image file
    , outputToConsole :: Bool -- ^ Flag to indicate whether to output ASCII art to console
    , outputFile :: Maybe FilePath -- ^ The path to the output file, if one should be created
    , flipAxis :: Maybe Axis -- ^ Optional axis along which to flip the image
    , rotationDegrees :: Maybe Int -- ^ Optional number of degrees by which to rotate the image
    , scalingFactor :: Maybe Double -- ^ Optional factor by which to scale the image
    , brightnessValue :: Maybe Int -- ^ Optional value to adjust the brightness of the image
    , invert :: Bool -- ^ Flag to indicate whether to invert the colors of the image
    }

-- | 'optionsParser' is a parser for the command line options. It uses the Options.Applicative library 
-- to define a parser that can handle each of the command line arguments and their possible values.
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
