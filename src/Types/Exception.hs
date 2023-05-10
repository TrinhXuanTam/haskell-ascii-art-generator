{-|
Module      : Types.Exception
Description : Defines custom exceptions for image handling
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module defines a custom exception type for handling errors in the application. 
It includes exceptions for errors that may occur when loading or decoding an image.
-}

module Types.Exception
  ( MyException(..)
  ) where

import Control.Exception

-- | Custom exception type for application-specific errors
data MyException
  = ImageLoadException String  -- ^ Exception for errors that occur when loading an image from a file
  | ImageDecodeException String -- ^ Exception for errors that occur when decoding an image from its binary representation
  deriving (Show)

instance Exception MyException