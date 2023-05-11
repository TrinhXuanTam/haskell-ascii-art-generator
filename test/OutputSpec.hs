module OutputSpec (spec) where

import Test.Hspec
import System.IO.Temp
import System.IO
import Output.Output
import Types.AsciiImage

spec :: Spec
spec = do
  describe "AsciiArtOutput" $ do
    it "should output to a file" $ do
      let asciiImage = AsciiImage [[0, 50], [255, 200]]
      let expectedOutput = " :\n@%\n"
      withSystemTempFile "asciiImage.txt" $ \tempFile hFile -> do
        hClose hFile -- Close the file so that it can be opened by 'outputAsciiArt'
        outputAsciiArt (FileOutput tempFile) asciiImage
        actualOutput <- readFile tempFile
        actualOutput `shouldBe` expectedOutput
        
    it "should overwrite the content of a file" $ do
      let asciiImage = AsciiImage [[0, 50], [255, 200]]
      let expectedOutput = " :\n@%\n"
      withSystemTempFile "asciiImage.txt" $ \tempFile hFile -> do
        hPutStr hFile "This should be overwritten"
        hClose hFile -- Close the file so that it can be opened by 'outputAsciiArt'
        outputAsciiArt (FileOutput tempFile) asciiImage
        actualOutput <- readFile tempFile
        actualOutput `shouldBe` expectedOutput