module Loaders.PngLoader where

import Loaders.Loader
import Control.Exception

import Loaders.JuicyPixelsTransformer
import Types.Exception

import Codec.Picture.Png (decodePng)

import qualified Data.ByteString as BS

newtype PngLoader = PngLoader {
    path :: FilePath
}

instance Loader PngLoader where
  loadImage (PngLoader path) = do
    result <- decodePng <$> BS.readFile path
    case result of
      Left err -> throw (ImageDecodeException ("Failed to read png image: " ++ err))
      Right img -> case convertDynamicImage img of
        Left err -> throw (ImageDecodeException ("Unsupported image format: " ++ err))
        Right myImg -> return myImg

