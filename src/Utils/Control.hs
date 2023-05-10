module Utils.Control where

import System.Exit (exitFailure)

-- A function to exit the program with a failure status code and a given error message.
exitWithFailure :: String -> IO ()
exitWithFailure errorMsg = do
  putStrLn errorMsg
  exitFailure

-- A function that exits with failure if a given condition is met.
conditionalExitWithFailure :: Bool -> String -> IO ()
conditionalExitWithFailure False _ = return ()
conditionalExitWithFailure True errorMsg = do
  exitWithFailure errorMsg
