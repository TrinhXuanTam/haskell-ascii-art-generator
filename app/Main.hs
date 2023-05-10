{-|
Module      : Main
Description : Main entry point for the ASCII Art Generator
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module serves as the entry point for the ASCII Art Generator program.
It imports and utilizes the 'runCli' function from the CLI.Controller module to drive the application.
-}
module Main
  ( main -- ^ The main entry point for the ASCII Art Generator
  ) where

import CLI.Controller

{-|
  The 'main' function serves as the entry point for the application.
  It executes the 'runCli' function from the CLI.Controller module.

  'runCli' is responsible for interpreting command line arguments and 
  executing the appropriate actions based on these arguments, such as 
  converting an image to ASCII art and applying filters.

  This function doesn't take any arguments and doesn't return any result. 
  It performs an IO action.
-}
main :: IO ()
main = runCli
