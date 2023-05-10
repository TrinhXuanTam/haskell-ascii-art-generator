{-|
Module      : Types.Image
Description : Provides Image and Pixel data types and conversion to greyscale
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module provides the 'Pixel' and 'Image' data types for representing color images. It also provides a typeclass 'ToPixel' for converting
different pixel representations to the 'Pixel' type. Additionally, it includes functions for converting an 'Image' to greyscale and to 'AsciiImage'.
-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types.Image
  ( Pixel(..)
  , Image(..)
  , ToPixel(..)
  ) where

import Types.AsciiImage

-- | A data type representing a pixel with red, green, and blue color components.
data Pixel =
  Pixel
    { red :: Int
    , green :: Int
    , blue :: Int
    }
  deriving (Eq, Show)

-- | A data type representing an image as a 2D list of 'Pixel's.
newtype Image =
  Image
    { pixels :: [[Pixel]]
    }
  deriving (Eq, Show)

-- | A typeclass for types that can be converted to 'Pixel'.
class ToPixel a where
  toPixel :: a -> Pixel

-- | Converts a 'Pixel' to a greyscale value using the formula 0.3 * red + 0.59 * green + 0.11 * blue
pixelToGreyscale :: Pixel -> Int
pixelToGreyscale pixel =
  let r = fromIntegral (red pixel)
      g = fromIntegral (green pixel)
      b = fromIntegral (blue pixel)
   in round (0.3 * r + 0.59 * g + 0.11 * b :: Double)

-- | Converts an 'Image' to a greyscale representation
toGreyscale :: Image -> [[Int]]
toGreyscale img = map (map pixelToGreyscale) (pixels img)

-- | 'AsciiArtConvertible' instance for 'Image'.
instance AsciiArtConvertible Image where
  toAsciiArt = AsciiImage . toGreyscale
