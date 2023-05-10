{-# LANGUAGE TypeSynonymInstances #-}

module Loaders.JuicyPixelsTransformer where

import Codec.Picture.Types
import Control.Exception
import Types.Exception
import Utils.Control

import Control.Monad (when)
import Data.Either (fromRight, isLeft)

import qualified Types.Image as IMG

convertDynamicImage :: DynamicImage -> Either String IMG.Image
convertDynamicImage (ImageY8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageY16 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageY32 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageYF img) = Right $ convertImageToIMG img
convertDynamicImage (ImageYA8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageYA16 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGB8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGB16 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBF img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBA8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageRGBA16 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageYCbCr8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageCMYK8 img) = Right $ convertImageToIMG img
convertDynamicImage (ImageCMYK16 img) = Right $ convertImageToIMG img
convertDynamicImage _ = Left "Unsupported image format"

convertImageToIMG :: (Pixel a, IMG.ToPixel a) => Image a -> IMG.Image
convertImageToIMG img =
  IMG.Image
    [ [IMG.toPixel (pixelAt img x y) | x <- [0 .. imageWidth img - 1]]
    | y <- [0 .. imageHeight img - 1]
    ]

instance IMG.ToPixel Pixel8 where
  toPixel p =
    let v = fromIntegral p
     in IMG.Pixel v v v

instance IMG.ToPixel Pixel16 where
  toPixel p =
    let v = fromIntegral p `div` 256
     in IMG.Pixel v v v

instance IMG.ToPixel Pixel32 where
  toPixel p =
    let v = fromIntegral p `div` 65536
     in IMG.Pixel v v v

instance IMG.ToPixel PixelF where
  toPixel p =
    let v = floor (p * 255)
     in IMG.Pixel v v v

instance IMG.ToPixel PixelYA8 where
  toPixel (PixelYA8 y _) =
    let v = fromIntegral y
     in IMG.Pixel v v v

instance IMG.ToPixel PixelYA16 where
  toPixel (PixelYA16 y _) =
    let v = fromIntegral y `div` 256
     in IMG.Pixel v v v

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

instance IMG.ToPixel PixelYCbCr8 where
  toPixel (PixelYCbCr8 y cb cr) =
    IMG.Pixel (fromIntegral y) (fromIntegral cb) (fromIntegral cr)

instance IMG.ToPixel PixelCMYK8 where
  toPixel (PixelCMYK8 c m y k) =
    let v = 255 - fromIntegral k
     in IMG.Pixel v v v

instance IMG.ToPixel PixelCMYK16 where
  toPixel (PixelCMYK16 c m y k) =
    let v = 65535 - fromIntegral k * 257
     in IMG.Pixel v v v
