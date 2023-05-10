module Types.Image where

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
  round
    (0.3 * fromIntegral (red pixel) + 0.59 * fromIntegral (green pixel) +
     0.11 * fromIntegral (blue pixel))

-- Convert an Image to a greyscale representation
toGreyscale :: Image -> [[Int]]
toGreyscale img = map (map pixelToGreyscale) (pixels img)

instance ToAsciiArt Image where
  toAsciiArt = AsciiImage . toGreyscale
