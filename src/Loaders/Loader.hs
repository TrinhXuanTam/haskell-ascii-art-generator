module Loaders.Loader where

import Loaders.Image
import Utils.Control
import Utils.FileSystem

class Loader a where
  loadImage :: a -> IO Image