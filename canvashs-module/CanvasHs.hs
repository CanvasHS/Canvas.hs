{- |
	De module CanvasHs maakt het mogelijk om op een eenvoudige manier te tekenen op een HTML5-canvaselement
	
	Op dit moment is de module in dummy-staat om de inrichting van de code duidelijk te hebben,
	de daadwerkelijke invulling (body) van de functies zal nog veranderen.
-}
module CanvasHs
(	
	installEventHandler
)  where

import CanvasHs.Data

installEventHandler :: 
		(userState -> EventData -> (userState, [Shape])) -- ^ event handler on current state and incoming event, that produces a tuple of the new user states and shapes to draw
	->	userState -- ^ start state
	-> 	IO ()
installEventHandler handl startState =
	

