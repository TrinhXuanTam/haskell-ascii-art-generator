module Filters.Filter where

data Axis = AxisX
          | AxisY

data Filter = InvertFilter
            | FlipFilter { axis :: Axis }
            | RotateFilter { degrees :: Int }
            | ScaleFilter { factor :: Double }
            | BrightnessFilter { value :: Int }

class FilterApplicable a where
    applyFilter :: a -> Filter -> a