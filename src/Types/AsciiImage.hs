module Types.AsciiImage (AsciiImage (..), AsciiArtConvertible(..), prettyPrint) where

import Filters.Filter
import Data.List (transpose)

newtype AsciiImage =
  AsciiImage
    { greyScaleValues :: [[Int]]
    }
  deriving (Show, Eq)

class AsciiArtConvertible a where
  toAsciiArt :: a -> AsciiImage

greyscaleCharacters :: String
greyscaleCharacters = " .:-=+*#%@"

-- Transforms a greyscale value into a character from the greyscaleCharacters string
greyscaleToChar :: Int -> Char
greyscaleToChar greyscaleValue =
  let charIndex = min (greyscaleValue `div` 25) (length greyscaleCharacters - 1)
   in greyscaleCharacters !! charIndex

-- Converts the AsciiImage to a string representation
prettyPrint :: AsciiImage -> String
prettyPrint img =
  let characterGrid = map (map greyscaleToChar) (greyScaleValues img)
   in unlines characterGrid


instance FilterApplicable AsciiImage where
  applyFilter (AsciiImage values) InvertFilter = AsciiImage $ map (map (255 -)) values
  applyFilter (AsciiImage values) (FlipFilter AxisX) = AsciiImage (reverse values)
  applyFilter (AsciiImage values) (FlipFilter AxisY) = AsciiImage (map reverse values)
  applyFilter (AsciiImage values) (BrightnessFilter brightness) =
    let adjustBrightness v = max 0 (min 255 (v + brightness))
    in AsciiImage $ map (map adjustBrightness) values
  applyFilter (AsciiImage values) (RotateFilter degrees) =
    let nearestMultipleOf90 = round (fromIntegral degrees / 90 :: Double) * 90 :: Int
        rotatedValues = case nearestMultipleOf90 `mod` 360 of
          90 -> reverse (transpose values)
          180 -> map reverse (reverse values)
          270 -> transpose (map reverse values)
          _ -> values
    in AsciiImage rotatedValues
  applyFilter (AsciiImage values) (ScaleFilter factor) =
    let origHeight = length values
        origWidth 
          | origHeight == 0 = 0 
          | otherwise = length (head values)
        newHeight = max 1 (round $ fromIntegral origHeight * factor) :: Int
        newWidth = max 1 (round $ fromIntegral origWidth * factor) :: Int
        getPixel y x = 
          let origX = min (origWidth - 1) (floor (fromIntegral x * fromIntegral origWidth / fromIntegral newWidth :: Double))
              origY = min (origHeight - 1) (floor ( fromIntegral y * fromIntegral origHeight / fromIntegral newHeight :: Double)) 
          in (values !! origY) !! origX
        newValues = [[getPixel y x | x <- [0..newWidth - 1]] | y <- [0..newHeight - 1]]
    in AsciiImage newValues