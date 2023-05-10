module Loaders.JpgLoader where

import Loaders.Loader
import Control.Exception

import Loaders.JuicyPixelsTransformer
import Types.Exception

import Codec.Picture.Jpg (decodeJpeg)

import qualified Data.ByteString as BS

-- | JpgLoader newtype for filepath
newtype JpgLoader = JpgLoader {
    path :: FilePath
}

-- | Loader instance for JpgLoader
instance Loader JpgLoader where
  loadImage (JpgLoader path) = do
    result <- decodeJpeg <$> BS.readFile path
    case result of
      Left err -> throw (ImageDecodeException ("Failed to read jpg image: " ++ err))
      Right img -> case convertDynamicImage img of
        Left err -> throw (ImageDecodeException ("Unsupported image format: " ++ err))
        Right myImg -> return myImg