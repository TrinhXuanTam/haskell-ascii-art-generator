{-# OPTIONS_GHC -Wno-orphans #-}

module Loaders.JuicyPixelsTransformer (convertDynamicImage) where

import Codec.Picture.Types

import qualified Types.Image as IMG

convertDynamicImage :: DynamicImage -> Either String IMG.Image
convertDynamicImage (ImageRGB8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGB16 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBF img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBA8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBA16 img) = Right $ convertImageToIMG img
convertDynamicImage _ = Left "Unsupported image format"

convertImageToIMG :: (Pixel a, IMG.ToPixel a) => Image a -> IMG.Image
convertImageToIMG img =
  IMG.Image
    [ [IMG.toPixel (pixelAt img x y) | x <- [0 .. imageWidth img - 1]]
    | y <- [0 .. imageHeight img - 1]
    ]

instance IMG.ToPixel PixelRGB8 where
  toPixel (PixelRGB8 r g b) =
    IMG.Pixel (fromIntegral r) (fromIntegral g) (fromIntegral b)

instance IMG.ToPixel PixelRGB16 where
  toPixel (PixelRGB16 r g b) =
    IMG.Pixel
      (fromIntegral r `div` 256)
      (fromIntegral g `div` 256)
      (fromIntegral b `div` 256)

instance IMG.ToPixel PixelRGBF where
  toPixel (PixelRGBF r g b) =
    IMG.Pixel (floor (r * 255)) (floor (g * 255)) (floor (b * 255))

instance IMG.ToPixel PixelRGBA8 where
  toPixel (PixelRGBA8 r g b _) =
    IMG.Pixel (fromIntegral r) (fromIntegral g) (fromIntegral b)

instance IMG.ToPixel PixelRGBA16 where
  toPixel (PixelRGBA16 r g b _) =
    IMG.Pixel
      (fromIntegral r `div` 256)
      (fromIntegral g `div` 256)
      (fromIntegral b `div` 256)