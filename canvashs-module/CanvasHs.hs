{-
	De module CanvasHs maakt het mogelijk om op een eenvoudige manier te tekenen op een HTML5-canvaselement
	
	Op dit moment is de module in dummy-staat om de inrichting van de code duidelijk te hebben,
	de daadwerkelijke invulling (body) van de functies zal nog veranderen.
-}
module CanvasHs
(	Output(..)
,	Point(Point)
)  where

-- | Dummy datamodel, wordt vervangen door uiteindelijke model
data Point 	= Point (Int, Int)
				deriving (Show, Eq)
-- | Dummy datamodel, wordt vervangen door uiteindelijke model				
data Output = Line Point Point
			| Rect Point Int Int
			| Circle Point Float
				deriving (Show, Eq)
			
