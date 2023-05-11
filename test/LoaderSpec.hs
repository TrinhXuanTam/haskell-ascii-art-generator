module LoaderSpec (spec) where

import Test.Hspec
import Control.Exception (try, SomeException)
import Loaders.Loader
import Types.Image
import Data.Either (isRight, isLeft)

spec :: Spec
spec = do
  describe "ImageLoader" $ do
    it "should load a PNG image" $ do
      result <- try $ loadImage (PngLoader "./resources/test.png") :: IO (Either SomeException Image)
      result `shouldSatisfy` isRight

    it "should load a JPG image" $ do
      result <- try $ loadImage (JpgLoader "./resources/spongebob.jpg") :: IO (Either SomeException Image)
      result `shouldSatisfy` isRight

    it "should throw an exception for non-existing PNG file" $ do
      result <- try $ loadImage (PngLoader "non-existing.png") :: IO (Either SomeException Image)
      result `shouldSatisfy` isLeft

    it "should throw an exception for non-existing JPG file" $ do
      result <- try $ loadImage (JpgLoader "non-existing.jpg") :: IO (Either SomeException Image)
      result `shouldSatisfy` isLeft