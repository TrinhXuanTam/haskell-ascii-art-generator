{-# LANGUAGE TypeSynonymInstances #-}

module Types.Image ( Pixel(..), Image(..), ToPixel(..) ) where

import Types.AsciiImage

data Pixel =
  Pixel
    { red :: Int
    , green :: Int
    , blue :: Int
    }
  deriving (Eq, Show)

newtype Image =
  Image
    { pixels :: [[Pixel]]
    }
  deriving (Eq, Show)

class ToPixel a where
  toPixel :: a -> Pixel

-- Convert a Pixel to a greyscale value using the formula 0.3 * red + 0.59 * green + 0.11 * blue
pixelToGreyscale :: Pixel -> Int
pixelToGreyscale pixel =
  let r = fromIntegral (red pixel)
      g = fromIntegral (green pixel)
      b = fromIntegral (blue pixel)
  in round (0.3 * r + 0.59 * g + 0.11 * b :: Double)

-- Convert an Image to a greyscale representation
toGreyscale :: Image -> [[Int]]
toGreyscale img = map (map pixelToGreyscale) (pixels img)

instance AsciiArtConvertible Image where
  toAsciiArt = AsciiImage . toGreyscale