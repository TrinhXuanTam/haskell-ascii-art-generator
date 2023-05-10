module Types.AsciiImage where

newtype AsciiImage =
  AsciiImage
    { greyScaleValues :: [[Int]]
    }
  deriving (Show, Eq)

class ToAsciiArt a where
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
