{-|
Module      : Types.AsciiImage
Description : Provides AsciiImage data type and conversion from greyscale to ASCII art
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module provides the 'AsciiImage' data type for representing ASCII art images. It also provides a typeclass 'AsciiArtConvertible' for converting
different image representations to the 'AsciiImage' type. Additionally, it includes functions for converting a greyscale image to ASCII art and for pretty-printing an 'AsciiImage'.
-}
module Types.AsciiImage
  ( AsciiImage(..)
  , AsciiArtConvertible(..)
  , prettyPrint
  ) where

import Data.List (transpose)
import Filters.Filter

-- | A data type representing an ASCII art image as a 2D list of greyscale values.
newtype AsciiImage =
  AsciiImage
    { greyScaleValues :: [[Int]]
    }
  deriving (Show, Eq)

-- | A typeclass for types that can be converted to 'AsciiImage'.
class AsciiArtConvertible a where
  toAsciiArt :: a -> AsciiImage

-- | A string of characters that represent the different shades of greyscale in ASCII art.
greyscaleCharacters :: String
greyscaleCharacters = " .:-=+*#%@"

-- | Converts a greyscale value to a character from the 'greyscaleCharacters' string.
-- The index of the character is calculated as the greyscale value divided by 25, capped at the length of 'greyscaleCharacters' minus 1.
greyscaleToChar :: Int -> Char
greyscaleToChar greyscaleValue =
  let charIndex = min (greyscaleValue `div` 25) (length greyscaleCharacters - 1)
   in greyscaleCharacters !! charIndex

-- | Converts an 'AsciiImage' to a string representation.
-- Each greyscale value in the image is converted to a character using 'greyscaleToChar', and the rows of characters are joined with newlines.
prettyPrint :: AsciiImage -> String
prettyPrint img =
  let characterGrid = map (map greyscaleToChar) (greyScaleValues img)
   in unlines characterGrid

-- | 'FilterApplicable' instance for 'AsciiImage'.
-- Each filter is applied to the greyscale values of the image in a different way.
instance FilterApplicable AsciiImage where
  -- InvertFilter inverts the colors of the image by subtracting each greyscale value from 255.                    
  applyFilter (AsciiImage values) InvertFilter =
    AsciiImage $ map (map (255 -)) values
  -- FlipFilter flips the image along the X or Y axis by reversing the rows or columns of greyscale values.
  applyFilter (AsciiImage values) (FlipFilter AxisX) =
    AsciiImage (reverse values)
  applyFilter (AsciiImage values) (FlipFilter AxisY) =
    AsciiImage (map reverse values)
  -- BrightnessFilter adjusts the brightness of the image by adding a constant value to each greyscale value.
  applyFilter (AsciiImage values) (BrightnessFilter brightness) =
    let adjustBrightness v = max 0 (min 255 (v + brightness))
     in AsciiImage $ map (map adjustBrightness) values
  -- RotateFilter rotates the image by a multiple of 90 degrees.
  applyFilter (AsciiImage values) (RotateFilter degrees) =
    let nearestMultipleOf90 =
          round (fromIntegral degrees / 90 :: Double) * 90 :: Int
        rotatedValues =
          case nearestMultipleOf90 `mod` 360 of
            90 -> reverse (transpose values)
            180 -> map reverse (reverse values)
            270 -> transpose (map reverse values)
            _ -> values
     in AsciiImage rotatedValues
  -- ScaleFilter scales the image by a certain factor.
  applyFilter (AsciiImage values) (ScaleFilter factor) =
    let origHeight = length values
        origWidth =
          if origHeight == 0
            then 0
            else length (head values)
        newHeight = max 1 (round $ fromIntegral origHeight * factor) :: Int
        newWidth = max 1 (round $ fromIntegral origWidth * factor) :: Int
        scaleCoordinate origSize newSize coord =
          min
            (origSize - 1)
            (floor
               (fromIntegral coord * fromIntegral origSize /
                fromIntegral newSize :: Double)) :: Int
        getPixel y x =
          let origX = scaleCoordinate origWidth newWidth x
              origY = scaleCoordinate origHeight newHeight y
           in (values !! origY) !! origX
        newValues =
          [ [getPixel y x | x <- [0 .. newWidth - 1]]
          | y <- [0 .. newHeight - 1]
          ]
     in AsciiImage newValues
