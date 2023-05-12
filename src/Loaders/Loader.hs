{-|
Module      : Loaders.Loader
Description : Handles loading of images
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module provides a way to load images from file paths. It defines a 'Loader' data type for different file formats and an 'ImageLoader' typeclass with a 'loadImage' function.
-}
module Loaders.Loader
  ( Loader(..)
  , ImageLoader(..)
  ) where

import Control.Exception
import Loaders.JuicyPixelsTransformer
import Types.Exception
import Types.Image

import Codec.Picture.Jpg (decodeJpeg)
import Codec.Picture.Png (decodePng)

import qualified Data.ByteString as BS

-- | Data type for different image loaders. Each constructor corresponds to a different image file format and takes a 'FilePath' as an argument.
data Loader
  = PngLoader FilePath -- ^ Constructor for loading PNG images. Takes the file path to the PNG image as an argument.
  | JpgLoader FilePath -- ^ Constructor for loading JPEG images. Takes the file path to the JPEG image as an argument.

-- | A typeclass for image loaders. Implementations should provide a way to load an image from a file.
class ImageLoader a where
  loadImage :: a -> IO Image

-- | A typeclass for image loaders. Implementations should provide a way to load an image from a file.
instance ImageLoader Loader where
  -- | Load a jpg image from a file. Throws an 'ImageDecodeException' if the file can't be read or the image format is unsupported.                                                                                                
  loadImage (JpgLoader filePath) = do
    result <- decodeJpeg <$> BS.readFile filePath
    case result of
      Left err ->
        throw (ImageDecodeException ("Failed to read jpg image: " ++ err))
      Right img ->
        case convertDynamicImage img of
          Left err ->
            throw (ImageDecodeException ("Unsupported image format: " ++ err))
          Right myImg -> return myImg
  -- | Load a png image from a file. Throws an 'ImageDecodeException' if the file can't be read or the image format is unsupported.
  loadImage (PngLoader filePath) = do
    result <- decodePng <$> BS.readFile filePath
    case result of
      Left err ->
        throw (ImageDecodeException ("Failed to read png image: " ++ err))
      Right img ->
        case convertDynamicImage img of
          Left err ->
            throw (ImageDecodeException ("Unsupported image format: " ++ err))
          Right myImg -> return myImg
