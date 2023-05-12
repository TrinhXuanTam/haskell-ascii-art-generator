module ImageSpec
  ( spec
  ) where

import Test.Hspec
import Types.AsciiImage
import Types.Image

spec :: Spec
spec = do
  describe "Pixel" $ do
    it "should be converted to the correct greyscale value" $ do
      let pixel = Pixel 255 255 255
      pixelToGreyscale pixel `shouldBe` 255
    it "should follow the greyscale conversion formula" $ do
      let pixel = Pixel 80 120 200
      pixelToGreyscale pixel `shouldBe`
        round (0.3 * 80 + 0.59 * 120 + 0.11 * 200 :: Double)
  describe "Image" $ do
    it "should be converted to the correct greyscale representation" $ do
      let pixel = Pixel 255 255 255
          image = Image [[pixel]]
      toGreyscale image `shouldBe` [[255]]
  describe "AsciiArtConvertible Image" $ do
    it "should convert an Image to AsciiImage correctly" $ do
      let pixel = Pixel 255 255 255
          image = Image [[pixel]]
          asciiImage = AsciiImage [[255]]
      toAsciiArt image `shouldBe` asciiImage
    it "should convert an Image with multiple pixels to AsciiImage correctly" $ do
      let pixel1 = Pixel 255 255 255
          pixel2 = Pixel 0 0 0
          image = Image [[pixel1, pixel2]]
          asciiImage = AsciiImage [[255, 0]]
      toAsciiArt image `shouldBe` asciiImage
