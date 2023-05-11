module JuicyPixelsTransformerSpec (spec) where

import Test.Hspec
import Codec.Picture.Types
import Loaders.JuicyPixelsTransformer
import qualified Types.Image as IMG
import Data.Word (Word8)
import Data.Either (isRight, isLeft)

spec :: Spec
spec = do
  describe "ToPixel instances" $ do
    it "should convert PixelRGB8 correctly" $ do
      let pixel = PixelRGB8 50 100 150
      let expected = IMG.Pixel 50 100 150
      IMG.toPixel pixel `shouldBe` expected

    it "should convert PixelRGB16 correctly" $ do
      let pixel = PixelRGB16 12800 25600 38400
      let expected = IMG.Pixel 50 100 150
      IMG.toPixel pixel `shouldBe` expected

    it "should convert PixelRGBF correctly" $ do
      let pixel = PixelRGBF (50/255) (100/255) (150/255)
      let expected = IMG.Pixel 50 100 150
      IMG.toPixel pixel `shouldBe` expected

    it "should convert PixelRGBA8 correctly" $ do
      let pixel = PixelRGBA8 50 100 150 200
      let expected = IMG.Pixel 50 100 150
      IMG.toPixel pixel `shouldBe` expected

    it "should convert PixelRGBA16 correctly" $ do
      let pixel = PixelRGBA16 12800 25600 38400 51200
      let expected = IMG.Pixel 50 100 150
      IMG.toPixel pixel `shouldBe` expected

    it "should convert PixelYCbCr8 correctly" $ do
      let pixel = PixelYCbCr8 50 100 150
      let (PixelRGB8 r g b) = convertPixel pixel :: PixelRGB8
      let expected = IMG.Pixel (fromIntegral r) (fromIntegral g) (fromIntegral b)
      IMG.toPixel pixel `shouldBe` expected