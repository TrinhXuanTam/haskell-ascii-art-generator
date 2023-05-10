module Types.Exception where

import Control.Exception

-- | Custom exception data type for handling errors in the application
data MyException
  = ImageLoadException String
  | ImageDecodeException String
  deriving (Show)

instance Exception MyException
