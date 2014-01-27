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

{-# LANGUAGE OverloadedStrings #-}
module CanvasHs.Server (start, sendText) where

import CanvasHs.Protocol
import CanvasHs.Data
import CanvasHs.Server.Static

-- Paths_canvashs is required to include static files
import qualified Network.WebSockets as WS
import Control.Monad (forever)
import qualified Data.Text as T
import Data.Maybe (isNothing)
import qualified Network.Wai.Handler.Warp as WRP (run)
import Data.Monoid
import Control.Concurrent
import Control.Exception (finally)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Control.Monad.Trans (liftIO, lift)
import Control.Applicative ((<$>))
import Debug.Trace
import Data.Time.Clock

-- | unsafePerformIO-hack function which is MVar of thread children
children :: MVar [MVar ()]
{-# NOINLINE children #-}
children = unsafePerformIO (newMVar [])

-- | unsafePerformIO-hack function which is IORef of connection
conn :: IORef (Maybe WS.Connection)
{-# NOINLINE conn #-}
conn = unsafePerformIO (newIORef Nothing)

{- | 
    Starts the server, this starts a httpserver on 8000 which will serve the static content
    And will start the websocket server which will handle the trafic between canvasses and user haskell

    The function argument is the function to be called when receiving data over a websocket
    It should return data to be send back, or Nothing if there is no data to send to the canvas
    note that when Nothing is returned to only way to send data over the websocket is to wait for
    input from the websocket
-}
start :: (T.Text -> IO (Maybe T.Text)) -> IO ()
start f =   do
                forkChild serverHttp -- the httpserver servers static files
                forkChild serverHandle -- runserver is a simple server for websockets
                time <- getCurrentTime   
                waitForChildren time -- wait until threads finish, this allows the process to be killed
                return ()
                where
                    serverHandle = liftIO $ WS.runServer "0.0.0.0" 8080 $ websockets f
                

serverHttp :: IO ()
serverHttp = do
                -- Serve static files
                dirFiles <- getDirectories "canvashs-client" >>= \dirs -> mapM getDirectoryFiles ("canvashs-client":dirs)
                forkIO $ WRP.run 8000 (httpget (concat dirFiles))
                return ()

websockets :: (T.Text -> IO (Maybe T.Text)) -> WS.PendingConnection -> IO ()
websockets f rq = do
                    cn <- WS.acceptRequest rq
                    atomicModifyIORef conn (\_ -> (Just cn, ()))
                    (Just initial) <- f $ T.pack "INIT"
                    WS.sendTextData cn initial
                    forever $ do
                        resp <- WS.receiveData cn >>= f
                        case resp of
                            Nothing -> return ()
                            Just m  -> WS.sendTextData cn m
                            
sendText :: T.Text -> IO ()
sendText t = readIORef conn >>= (\c -> case c of
                Nothing -> error "No open connection, cannot sendText"
                Just cn -> WS.sendTextData cn t
             )
                            
-- Code below manages the threads so all threads stop when the main thread is stopped
waitForChildren :: UTCTime -> IO ()
waitForChildren timeStarted = do
    time <- getCurrentTime
--    cs <- takeMVar children
    if (diffUTCTime time timeStarted > 30) 
        then trace "Stopping!" $ return () 
        else  -- $ case cs of
--                []   -> return ()
--              m:ms -> do
--              putMVar children ms
--              takeMVar m
                waitForChildren timeStarted

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())
