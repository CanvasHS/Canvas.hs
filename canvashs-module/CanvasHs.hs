{- |
    De module CanvasHs maakt het mogelijk om op een eenvoudige manier te tekenen op een HTML5-canvaselement
    
    Op dit moment is de module in dummy-staat om de inrichting van de code duidelijk te hebben,
    de daadwerkelijke invulling (body) van de functies zal nog veranderen.
-}
module CanvasHs
(
    installEventHandler,
    shape
)  where

import CanvasHs.Data
import CanvasHs.Server
import CanvasHs.Launch
import CanvasHs.Protocol

import qualified Data.Text as T
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Control.Monad.Trans (liftIO, lift)
import System.IO (readFile, writeFile)
import qualified Data.ByteString as BS (readFile, writeFile)
import Data.Maybe (catMaybes)
import Control.Concurrent.Timer
import Control.Concurrent.Suspend (msDelay)
import Control.Applicative ((<$>))

import qualified Network.WebSockets as WS

-- | type of the user handler, accepts a state and an Event and produces a tuple of the new State and an Output
type Callback a = (a -> Event -> (a, Output))

-- | Our internal state, holds the user state and a refrence tot he user handler
data State a =  State   {extState :: a
                        ,callback :: Callback a
                        }

                      
                        
-- | Start CanvasHs om grafische weergave mogelijk te maken. registreert de event handler en de start state van
--	 de de user.
installEventHandler :: 
        Callback userState -- ^ event handler on current state and incoming event, that produces a tuple of the new user state and ouput to process
    ->  userState -- ^ start state
    ->  IO ()
installEventHandler handl startState = do
    store <- newIORef (State{extState=startState, callback=handl})
    launchBrowser "http://localhost:8000"
    start $ handleWSInput store
    return ()
    
-- | convenience function to output just a shape
shape :: Shape -> Output
shape s = Out (Just s, [])
    
-- | handles input from the canvas
handleWSInput :: IORef (State a) -> T.Text -> IO (Maybe T.Text)
handleWSInput st ip = handleEvent st $ decode ip

-- | handles an event, it is fed through the handler, the newstate is saved and the resulting 
handleEvent :: IORef (State a) -> Event -> IO (Maybe T.Text)
handleEvent st e    = do
                        curState <- readIORef st
                        let
                            (newState, output) = (callback curState) (extState curState) e
                        atomicModifyIORef st (\_ -> (curState{extState=newState}, ())) --update de state
                        case output of 
                               (Out (s,a))  -> (doActions st a) >>= (\a' -> return $ Just $ encode (s,a'))
                               (Block a)    -> doBlockingAction a >>= (handleEvent st)
                               
-- | handles non blocking actions. The result will be a list of non blocking actions which were not handled by haskell
-- | and should be sent to the javascript
doActions :: IORef (State a) -> [Action] -> IO [Action]
doActions st [] = return []
doActions st xs =  (sequence $ map doAction xs) >>= (return . catMaybes)
                    --the above line, sequence: [IO Maybe Action] -> IO [Maybe Action], catMaybes: [Maybe Action] -> [Action] (discards Nothings)
                where
                    doAction :: Action -> IO (Maybe Action)
                    doAction (SaveFileString p c)   = writeFile p c >> return Nothing
                    doAction (SaveFileBinary p c)   = BS.writeFile p c >> return Nothing
                    doAction (Timer ms id)          = repeatedTimer (liftIO $ handleTick st id >> return ()) (msDelay $ fromIntegral ms) >> return Nothing
                    -- Other actions fall through and are encoded by encode
                    doAction a                      = return $ Just a
                    
                               
-- | handles blocking actions. The actions are executed and the corresponding Event is returned
doBlockingAction :: BlockingAction -> IO (Event)
doBlockingAction (LoadFileString p) = readFile p >>= (\c -> return (FileLoadedString p c))
doBlockingAction (LoadFileBinary p) = BS.readFile p >>= (\c -> return (FileLoadedBinary p c))
-- TODO: upload
doBlockingAction _ = return StartEvent

handleTick :: IORef (State a) -> String -> IO ()
handleTick st id = handleEvent st (Tick id) >>= (\mt -> case mt of
                                                        Nothing -> return ()
                                                        Just t -> sendText t
                                                )
                                   


