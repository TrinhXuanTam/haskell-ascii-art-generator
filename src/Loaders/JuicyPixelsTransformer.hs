{-|
Module      : Loaders.JuicyPixelsTransformer
Description : Transforms images from JuicyPixels to a custom Image type
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module provides a function to convert images from the 'DynamicImage' type provided by the JuicyPixels library to a custom 'Image' type. 
It also defines instances of the 'ToPixel' typeclass for various JuicyPixels pixel types.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Loaders.JuicyPixelsTransformer
  ( convertDynamicImage
  ) where

import Codec.Picture.Types

import qualified Types.Image as IMG

-- | Converts a 'DynamicImage' to a custom 'Image'. Returns 'Left' with an error message if the image format is unsupported.
convertDynamicImage :: DynamicImage -> Either String IMG.Image
convertDynamicImage (ImageRGB8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGB16 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBF img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBA8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBA16 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageYCbCr8 img) = Right $ convertImageToIMG img
convertDynamicImage _ = Left "Unsupported image format"

-- | Converts an 'Image' of a certain pixel type to a custom 'Image'. This function is used internally in 'convertDynamicImage'.
convertImageToIMG :: (Pixel a, IMG.ToPixel a) => Image a -> IMG.Image
convertImageToIMG img =
  IMG.Image
    [ [IMG.toPixel (pixelAt img x y) | x <- [0 .. imageWidth img - 1]]
    | y <- [0 .. imageHeight img - 1]
    ]

-- | 'ToPixel' instance for 'PixelRGB8'. 
-- 'PixelRGB8' is a pixel format with 8 bits per channel in the RGB color space. 
-- This instance provides a method to convert 'PixelRGB8' to a custom 'Pixel' type.
instance IMG.ToPixel PixelRGB8 where
  toPixel (PixelRGB8 r g b) =
    IMG.Pixel (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | 'ToPixel' instance for 'PixelRGB16'. 
-- 'PixelRGB16' is a pixel format with 16 bits per channel in the RGB color space. 
-- This instance provides a method to convert 'PixelRGB16' to a custom 'Pixel' type.
instance IMG.ToPixel PixelRGB16 where
  toPixel (PixelRGB16 r g b) =
    IMG.Pixel
      (fromIntegral r `div` 256)
      (fromIntegral g `div` 256)
      (fromIntegral b `div` 256)

-- | 'ToPixel' instance for 'PixelRGBF'. 
-- 'PixelRGBF' is a pixel format with floating point numbers per channel in the RGB color space. 
-- This instance provides a method to convert 'PixelRGBF' to a custom 'Pixel' type.
instance IMG.ToPixel PixelRGBF where
  toPixel (PixelRGBF r g b) =
    IMG.Pixel (floor (r * 255)) (floor (g * 255)) (floor (b * 255))

-- | 'ToPixel' instance for 'PixelRGBA8'. 
-- 'PixelRGBA8' is a pixel format with 8 bits per channel in the RGBA color space. 
-- This instance provides a method to convert 'PixelRGBA8' to a custom 'Pixel' type.
instance IMG.ToPixel PixelRGBA8 where
  toPixel (PixelRGBA8 r g b _) =
    IMG.Pixel (fromIntegral r) (fromIntegral g) (fromIntegral b)

-- | 'ToPixel' instance for 'PixelRGBA16'. 
-- 'PixelRGBA16' is a pixel format with 16 bits per channel in the RGBA color space. 
-- This instance provides a method to convert 'PixelRGBA16' to a custom 'Pixel' type.
instance IMG.ToPixel PixelRGBA16 where
  toPixel (PixelRGBA16 r g b _) =
    IMG.Pixel
      (fromIntegral r `div` 256)
      (fromIntegral g `div` 256)
      (fromIntegral b `div` 256)

-- | 'ToPixel' instance for 'PixelYCbCr8'. 
-- 'PixelYCbCr8' is a pixel format with 8 bits per channel in the YCbCr color space. 
-- This instance provides a method to convert 'PixelYCbCr8' to a custom 'Pixel' type.
instance IMG.ToPixel PixelYCbCr8 where
  toPixel p =
    let (PixelRGB8 r g b) = convertPixel p
     in IMG.Pixel (fromIntegral r) (fromIntegral g) (fromIntegral b)
