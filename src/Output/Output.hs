module Output.Output ( Output(..), AsciiArtOutput(..) ) where

import System.IO
import Types.AsciiImage

data Output
  = FileOutput FilePath
  | CliOutput

class AsciiArtOutput a where
  outputAsciiArt :: a -> AsciiImage -> IO ()

instance AsciiArtOutput Output where
  outputAsciiArt CliOutput asciiImage = putStrLn (prettyPrint asciiImage)
  outputAsciiArt (FileOutput outputPath) asciiImage = do
    handle <- openFile outputPath WriteMode
    hPutStr handle (prettyPrint asciiImage)
    hClose handle
