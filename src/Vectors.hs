module Vectors where

data Point = Point { x :: Double,
                     y :: Double
                   } deriving (Eq, Show)

data Vector = Vector { start :: Point,
                       end :: Point
                     } deriving (Eq, Show)


len :: Vector -> Double
len (Vector {start = (Point{x = x1, y = y1}), end = (Point{x = x2, y = y2})}) 
        = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2

