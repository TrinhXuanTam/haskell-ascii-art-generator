module Main (main) where

import Test.Hspec

import qualified FileSystemSpec
import qualified AsciiImageSpec
import qualified ImageSpec
import qualified OutputSpec
import qualified LoaderSpec
import qualified JuicyPixelsTransformerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Utility Functions for File System Operations" FileSystemSpec.spec
  describe "Image and Pixel Operations" ImageSpec.spec
  describe "ASCII Image Operations" AsciiImageSpec.spec
  describe "Output Operations" OutputSpec.spec
  describe "Image Loading Operations" LoaderSpec.spec
  describe "JuicyPixels Transformation Operations" JuicyPixelsTransformerSpec.spec