{- |
	De module CanvasHs maakt het mogelijk om op een eenvoudige manier te tekenen op een HTML5-canvaselement
	
	Op dit moment is de module in dummy-staat om de inrichting van de code duidelijk te hebben,
	de daadwerkelijke invulling (body) van de functies zal nog veranderen.
-}
module CanvasHs
(	
	installEventHandler, basicTest
)  where

import CanvasHs.Data
import CanvasHs.Server
import CanvasHs.Launch

import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Control.Monad.Trans (liftIO, lift)

data State a = 	State 	{extState :: a
						,callback :: (a -> EventData -> (a, [Shape]))
						}

-- | Start CanvasHs om grafische weergave mogelijk te maken. registreert de event handler en de start state van
--	 de de user.
installEventHandler :: 
		(userState -> EventData -> (userState, [Shape])) -- ^ event handler on current state and incoming event, that produces a tuple of the new user states and shapes to draw
	->	userState -- ^ start state
	-> 	IO ()	
installEventHandler handl startState = do
	store <- newIORef (State{extState=startState, callback=handl})
	-- server starten, dingen openen, store rondgooien, jwz
	return ()

-- This is a basic server session without the state handler
basicTest :: IO ()
basicTest = do
	launchBrowser "http://localhost:8000"
	start -- starts server


