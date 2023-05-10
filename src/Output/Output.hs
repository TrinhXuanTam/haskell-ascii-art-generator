module Output.Output where

import System.IO
import Types.AsciiImage

data Output
  = FileOutput
      { path :: FilePath
      }
  | CliOutput

class AsciiArtOutput a where
  outputAsciiArt :: a -> AsciiImage -> IO ()

instance AsciiArtOutput Output where
  outputAsciiArt CliOutput asciiImage = putStrLn (prettyPrint asciiImage)
  outputAsciiArt (FileOutput path) asciiImage = do
    handle <- openFile path WriteMode
    hPutStr handle (prettyPrint asciiImage)
    hClose handle
