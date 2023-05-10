module Loaders.Image where

data Pixel = Pixel
  { red :: Int
  , green :: Int
  , blue :: Int
  } deriving (Eq, Show)

data Image = Image
  { pixels :: [[Pixel]]
  } deriving (Eq, Show)

class ToPixel a where
  toPixel :: a -> Pixel

class ToImage a where
  toImage :: a -> Image