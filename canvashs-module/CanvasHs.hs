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

import Data.IORef (IORef, newIORef, atomicWriteIORef, readIORef)
import Control.Monad.Trans (liftIO, lift)

data State a = 	State 	{extState :: a
						,callback :: (a -> Event -> (a, [Shape]))
						}

-- | Start CanvasHs om grafische weergave mogelijk te maken. registreert de event handler en de start state van
--	 de de user.
installEventHandler :: 
		(userState -> Event -> (userState, [Shape])) -- ^ event handler on current state and incoming event, that produces a tuple of the new user states and shapes to draw
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
							curState <- readState st
							let
								(newState, shapes) = (callback curState) (extState curState) $ decode ip
							updateState st curState{extState=newState}
							return $ Just $ encode $ head shapes  -- Just $ encode shapes
	
-- | updates the stored State to the new State and returns the new State
updateState :: IORef (State a) -> (State a) -> IO (State a)
updateState io st = do
						atomicWriteIORef io st
						return st

-- | read the State stored
readState :: IORef (State a) -> IO (State a)
readState = readIORef

