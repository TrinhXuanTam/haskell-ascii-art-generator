module AsciiImageSpec
  ( spec
  ) where

import Filters.Filter
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Types.AsciiImage

instance Arbitrary AsciiImage where
  arbitrary = AsciiImage <$> vectorOf 1 (vectorOf 1 (choose (0, 255)))

spec :: Spec
spec = do
  describe "AsciiImage" $ do
    it "should convert greyscale to correct character" $ do
      let greyscaleValue = 125
      greyscaleToChar greyscaleValue `shouldBe` greyscaleCharacters !!
        (125 `div` 25)
    it "should pretty print correctly" $ do
      let asciiImage = AsciiImage [[0, 50], [255, 200]]
      prettyPrint asciiImage `shouldBe` " :\n@%\n"
  describe "FilterApplicable AsciiImage" $ do
    prop "InvertFilter should invert colors" $ \asciiImage ->
      let inverted = applyFilter asciiImage InvertFilter
          original = greyScaleValues asciiImage
          invertedValues = greyScaleValues inverted
       in all
            (\(orig, inv) -> orig + inv == 255)
            (zip (concat original) (concat invertedValues))
