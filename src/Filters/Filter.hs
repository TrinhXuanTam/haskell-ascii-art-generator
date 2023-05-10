module Filters.Filter (Axis(..), Filter(..), FilterApplicable(..)) where

data Axis = AxisX
          | AxisY

data Filter = InvertFilter
            | FlipFilter Axis 
            | RotateFilter Int
            | ScaleFilter Double
            | BrightnessFilter Int

class FilterApplicable a where
    applyFilter :: a -> Filter -> a