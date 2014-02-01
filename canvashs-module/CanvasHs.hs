-- Canvas.Hs, control javascript canvas with Haskell
-- Copyright (C) 2013, Lennart Buit, Joost van Doorn, Pim Jager, Martijn Roo,
-- Thijs Scheepers
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
-- USA

{- |
    The CanvasHS module allows haskell programmers to easily create graphical programs using event diven IO.    
    Using the installEventHandler function the user can register an event handler which will process incoming 
    events and with the current state in mind, will result in a new state and output, which could be graphical,
    actions (IO, timers and others) or both.
-}
module CanvasHs
(
    installEventHandler,
    shape,
    actions
)  where

import CanvasHs.Data
import CanvasHs.Server
import CanvasHs.Launch
import CanvasHs.Protocol
import CanvasHs.Shutdown as Shutdown (addEnd) 
    -- the serverthread will call shutdown when exiting. We use this to stop the timers

import Control.Monad.Trans (liftIO, lift)
import System.IO (readFile, writeFile)
import System.Environment (getArgs)
import Data.Maybe (catMaybes)
import Control.Concurrent.Timer as Timer
import Control.Concurrent.Suspend (msDelay)
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BSL (readFile, writeFile)
import qualified Data.ByteString.UTF8 as BU
import Control.Concurrent.MVar
import qualified Control.Monad.State.Strict as S (runStateT, StateT, get, put)

import qualified Network.WebSockets as WS

-- | type of the user handler. It accepts a state and an 'Event' and produces a tuple of the new state and an 'Output'
type Callback a = (a -> Event -> (a, Output))

-- | Our internal state, holds the user state and a refrence to the user handler
data State a =  State   {extState :: a
                        ,callback :: Callback a
                        ,timers :: Map.Map String TimerIO
                        }

                      
                        
-- | Registers an event handler and starts CanvasHs. This will start the needed servers (weboscket and http) and will open a browser window.
installEventHandler :: 
        Callback userState -- ^ event handler on current state and incoming event, that produces a tuple of the new user state and ouput to process: (userState -> Event -> (userState, Output))
    ->  userState -- ^ start state
    ->  IO ()
installEventHandler handl startState = do
    args <- getArgs
    if ("--prevent-browser-launch" `elem` args)
        then return ()
        else launchBrowser "http://localhost:8000"
    store <- newMVar (State{extState=startState, callback=handl, timers=Map.empty})
    start $ handleWSInput store
    return ()
    
-- | convenience function to create an 'Output' of just a 'Shape'
shape :: Shape -> Output
shape s = Out (Just s, [])

-- | convenience function to create an 'Output' of just a list of 'Action's
actions :: [Action] -> Output
actions a = Out (Nothing, a)

-- | handles input from the canvas
handleWSInput :: MVar (State a) -> BU.ByteString -> IO (Maybe BU.ByteString)
handleWSInput st ip = handleEvent st $ decode ip

-- | handles an event, it is fed through the handler, the newstate is saved and the resulting 
handleEvent :: MVar (State a) -> Event -> IO (Maybe BU.ByteString)
handleEvent st e    = do
                        cst <- takeMVar st
                        let
                            (userState, output) = (callback cst) (extState cst) e
                            cst' = cst{extState = userState}
                        case output of 
                               (Out (s,acs)) -> do
                                                    (acs', cst'') <- doActions st cst' acs
                                                    putMVar st cst''
                                                    return $ Just $ encode (s,acs')
                                    -- the 'Output' is a tuple of a 'Shape' to draw and a list of 'Action's to execute
                               (Block a)    -> putMVar st cst' >> doBlockingAction a >>= (handleEvent st)
                                    -- the 'Output' is a 'BlockingAction'
                               
-- | handles non blocking 'Action's. The result will be a list of non blocking actions which were not handled by 
--   doActions and which should be sent to the javascript
--   The MVar on state is passed to the Timer actions, and will only be used in speerate (timer) threads
doActions :: MVar (State a) -> State a -> [Action] -> IO ([Action], State a)
doActions st cst [] = return ([], cst)
doActions st cst xs = S.runStateT (ap xs) cst >>= \(ma, cst') -> return (catMaybes ma, cst')                 
    where
        ap :: [Action] -> S.StateT (State a) IO [Maybe Action]
        ap []       = return []
        ap (a:acs)  = do 
                        st <- S.get
                        (m, st') <- liftIO $ doAction st a
                        S.put st'
                        rst <- ap acs
                        return $ m : rst
        doAction :: State a -> Action -> IO (Maybe Action, State a)
        doAction cst (SaveFileString p c)   = writeFile p c >> return (Nothing, cst)
        doAction cst (SaveFileBinary p c)   = BSL.writeFile p c >> return (Nothing, cst)
        doAction cst (Timer ms id)          = do 
                                                let tms = timers $ cst
                                                if(Map.member id tms) then (stopTimer id tms) else (return ())
                                                timer <- liftIO $ repeatedTimer (handleTick st id) (msDelay $ fromIntegral ms)
                                                liftIO $ Shutdown.addEnd $ Timer.stopTimer timer
                                                return (Nothing, cst{timers=Map.insert id timer tms})
        doAction cst (StopTimer id)         = do
                                                let tms = timers cst
                                                liftIO $ stopTimer id tms
                                                return (Nothing, cst{timers=Map.delete id tms})
            -- Other actions fall through and should be handled by javascript
        doAction cst a                      = return (Just a, cst)
            --stopTimer will stop a timer from the timermap
        stopTimer :: String -> Map.Map String TimerIO -> IO ()
        stopTimer id tms = case Map.lookup id tms of
                            Nothing -> return ()
                            Just t -> Timer.stopTimer t
                                        
-- | handles blocking actions. The actions are executed and the corresponding Event is returned
doBlockingAction :: BlockingAction -> IO (Event)
doBlockingAction (LoadFileString p) = readFile p >>= (\c -> return (FileLoadedString p c))
doBlockingAction (LoadFileBinary p) = BSL.readFile p >>= (\c -> return (FileLoadedBinary p c))

-- | Handles a Tick from a Timer by calling handleEvent with a Tick event and sending the result (if any)
--   to javascript
handleTick :: MVar (State a) -> String -> IO ()
handleTick st id = handleEvent st (Tick id) >>= (\mt -> case mt of
                                                        Nothing -> return ()
                                                        Just t -> sendText t
                                                )
                                   


