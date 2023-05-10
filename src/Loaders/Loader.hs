module Loaders.Loader (
  Loader(..),
  ImageLoader(..)
) where

import Control.Exception
import Loaders.JuicyPixelsTransformer
import Types.Exception
import Types.Image

import Codec.Picture.Jpg (decodeJpeg)
import Codec.Picture.Png (decodePng)

import qualified Data.ByteString as BS

data Loader
  = PngLoader
      { path :: FilePath
      }
  | JpgLoader
      { path :: FilePath
      }

class ImageLoader a where
  loadImage :: a -> IO Image

-- | Loader instance for JpgLoader
instance ImageLoader Loader where
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
