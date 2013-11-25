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
import CanvasHs.Server
import CanvasHs.Launch
import CanvasHs.Protocol

import qualified Data.Text as T

import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Control.Monad.Trans (liftIO, lift)

-- | type of the user handler, accepts a state and an Event and produces a tuple of the new State and an Output
type Callback a = (a -> Event -> (a, Output))

-- | Our internal state, holds the user state and a refrence tot he user handler
data State a = 	State 	{extState :: a
						,callback :: Callback a
						}

                      
                        
-- | Start CanvasHs om grafische weergave mogelijk te maken. registreert de event handler en de start state van
--	 de de user.
installEventHandler :: 
		Callback userState -- ^ event handler on current state and incoming event, that produces a tuple of the new user state and ouput to process
	->	userState -- ^ start state
	-> 	IO ()	
installEventHandler handl startState = do
	store <- newIORef (State{extState=startState, callback=handl})
	launchBrowser "http://localhost:8000"
	start $ handleInput store
	return ()
	
-- | handles input from the canvas, calls the handler on it and sends the result back to the canvas
handleInput :: IORef (State a) -> T.Text -> IO (Maybe T.Text)
handleInput st ip	= do
							curState <- readIORef st
							let
								(newState, output) = (callback curState) (extState curState) $ decode ip
							atomicModifyIORef st (\_ -> (curState{extState=newState}, ())) --update de state
							return $ Just $ encode output

