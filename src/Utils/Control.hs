{-|
Module      : Utils.Control
Description : Provides utility functions to control program execution flow
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module provides utility functions to control the execution flow of the program.
It includes functions to exit the program with a failure status code and to conditionally exit with a failure.
-}
module Utils.Control
  ( exitWithFailure
  , conditionalExitWithFailure
  ) where

import System.Exit (exitFailure)

-- | 'exitWithFailure' is a function that takes an error message, prints it to the console, 
--   and then exits the program with a failure status code.
exitWithFailure :: String -> IO ()
exitWithFailure errorMsg = do
  putStrLn errorMsg
  exitFailure

-- | 'conditionalExitWithFailure' is a function that takes a boolean condition and an error message.
--   If the condition is 'True', it calls 'exitWithFailure' with the provided error message.
--   If the condition is 'False', it does nothing.
conditionalExitWithFailure :: Bool -> String -> IO ()
conditionalExitWithFailure False _ = return ()
conditionalExitWithFailure True errorMsg = do
  exitWithFailure errorMsg
