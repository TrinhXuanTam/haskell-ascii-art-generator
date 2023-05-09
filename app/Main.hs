{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Options.Applicative

-- A data type to hold the command line options
data Options = Options
  { sourcePath :: FilePath
  }

-- A parser for the command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
      ( long "src"
     <> metavar "PATH"
     <> help "The path to the source image file" )

main :: IO ()
main = do
  options <- execParser $ info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "A command-line utility that takes an image from a provided input path, converts the image into ASCII art, and optionally applies filters."
   <> header "ASCII Art Generator" )