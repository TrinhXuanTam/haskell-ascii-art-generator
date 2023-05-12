{-|
Module      : Filters.Filter
Description : Filters for the ASCII Art Generator
Copyright   : (c) 2023
License     : BSD3
Maintainer  : trinhxu2@fit.cvut.cz
Stability   : experimental
Portability : POSIX

This module defines the types and classes related to filters that can be applied to ASCII art in the ASCII Art Generator.
It includes the 'Axis' type for specifying flip directions, the 'Filter' type for specifying different kinds of filters, 
and the 'FilterApplicable' typeclass for types that can have filters applied to them.

The main entry point for applying filters is the 'applyFilter' function.
-}
module Filters.Filter
  ( Axis(..)
  , Filter(..)
  , FilterApplicable(..)
  ) where

-- | 'Axis' is a data type that specifies the axis along which an image can be flipped. 
data Axis
  = AxisX -- ^ Represents the X-axis
  | AxisY -- ^ Represents the Y-axis

-- | 'Filter' is a data type that represents different kinds of filters that can be applied to an image.
data Filter
  = InvertFilter -- ^ Represents a filter that inverts the colors of an image
  | FlipFilter Axis -- ^ Represents a filter that flips an image along a specified axis
  | RotateFilter Int -- ^ Represents a filter that rotates an image by a specified number of degrees
  | ScaleFilter Double -- ^ Represents a filter that scales an image by a specified factor
  | BrightnessFilter Int -- ^ Represents a filter that adjusts the brightness of an image by a specified value

-- | 'FilterApplicable' is a typeclass for types that can have filters applied to them. 
-- The 'applyFilter' function takes an instance of 'FilterApplicable', a 'Filter', and applies the filter to the instance.
class FilterApplicable a where
  applyFilter :: a -> Filter -> a
