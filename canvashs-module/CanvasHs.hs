module CanvasHs
(	Output(..)
,	Point(Point)
)  where

data Point 	= Point (Int, Int)
				deriving (Show, Eq)
				
data Output = Line Point Point
			| Rect Point Int Int
			| Circle Point Float
				deriving (Show, Eq)
			
