module Loaders.Loader where

import Types.Image
import Types.Exception
import Utils.Control
import Utils.FileSystem
import Control.Exception
import Loaders.JuicyPixelsTransformer

import Codec.Picture.Jpg (decodeJpeg)
import Codec.Picture.Png (decodePng)

import qualified Data.ByteString as BS

data Loader = PngLoader { path :: FilePath }
            | JpgLoader { path :: FilePath }

class ImageLoader a where
  loadImage :: a -> IO Image


-- | Loader instance for JpgLoader
instance ImageLoader Loader where
  loadImage (JpgLoader path) = do
    result <- decodeJpeg <$> BS.readFile path
    case result of
      Left err -> throw (ImageDecodeException ("Failed to read jpg image: " ++ err))
      Right img -> case convertDynamicImage img of
        Left err -> throw (ImageDecodeException ("Unsupported image format: " ++ err))
        Right myImg -> return myImg
  loadImage (PngLoader path) = do
    result <- decodePng <$> BS.readFile path
    case result of
      Left err -> throw (ImageDecodeException ("Failed to read png image: " ++ err))
      Right img -> case convertDynamicImage img of
        Left err -> throw (ImageDecodeException ("Unsupported image format: " ++ err))
        Right myImg -> return myImg