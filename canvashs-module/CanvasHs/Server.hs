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

{- |
    This module exposes the haskell server for CanvasHs, it functions as both the 
    http server and the websockets server. The websocket server will be launched on port 8080
    and the http server on 8000.
    The module exposes a sendText function wich will send textdata (ByteString) over the websocket connection
    and a start function which will start both servers and has a funtion argument which will be called
    to handle incoming data over the websockets connection. 
    The static files served by the http-server are dynamilcly loaded from the canvashs-client directory.
-}
module CanvasHs.Server (start, sendText) where

import CanvasHs.Server.Static

import qualified Network.WebSockets as WS
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as WRP (run)
import Data.Monoid
import Control.Concurrent
import Control.Exception (finally)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Control.Monad.Trans (liftIO)

-- | unsafePerformIO-hack function which is MVar of thread children
children :: MVar [MVar ()]
{-# NOINLINE children #-}
children = unsafePerformIO (newMVar [])

-- | unsafePerformIO-hack function which is IORef of connection
conn :: IORef (Maybe WS.Connection)
{-# NOINLINE conn #-}
conn = unsafePerformIO (newIORef Nothing)

-- | Starts the server, this starts a httpserver on 8000 which will serve the static content
--   And will start the websocket server which will handle the trafic between canvas and user haskell
--
--   The function argument is the function to be called when receiving data over a websocket
--   It should return data to be send back, or Nothing if there is no data to send to the canvas.
start :: (T.Text -> IO (Maybe T.Text)) -> IO ()
start f =   do
                forkChild serverHttp
                forkChild $ liftIO $ WS.runServer "0.0.0.0" 8080 $ websockets f
                waitForChildren --this blocks so both the websockets and httpserver will terminate when main terminates
                return ()
                
-- | Starts the httpserver, which will serve the static files from canvashs-client
serverHttp :: IO ()
serverHttp = do
                dirFiles <- getDirectories "canvashs-client" >>= \dirs -> mapM getDirectoryFiles ("canvashs-client":dirs)
                forkIO $ WRP.run 8000 (httpget (concat dirFiles))
                return ()
                
-- | Starts the swebsockets server, which will handle incoming websockets data using the provided handler
websockets :: (T.Text -> IO (Maybe T.Text)) -> WS.PendingConnection -> IO ()
websockets f rq = do
                    cn <- WS.acceptRequest rq
                    atomicModifyIORef conn (\_ -> (Just cn, ()))
                    (Just initial) <- f $ T.pack "INIT" -- Tell the handler that the websockets connection has opened
                    WS.sendTextData cn initial
                    forever $ do
                        resp <- WS.receiveData cn >>= f
                        case resp of
                            Nothing -> return ()
                            Just m  -> WS.sendTextData cn m

-- | sends textdata over the webscokets connection, will trigge an error when the connection is not open                          
sendText :: T.Text -> IO ()
sendText t = readIORef conn >>= (\c -> case c of
                Nothing -> error "No open connection, cannot sendText"
                Just cn -> WS.sendTextData cn t
             )
                            
-- Code below manages the threads so all threads stop when the main thread is stopped
waitForChildren :: IO ()
waitForChildren = do
    cs <- takeMVar children
    case cs of
        []   -> return ()
        m:ms -> do
           putMVar children ms
           takeMVar m
           waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    mvar <- newEmptyMVar
    childs <- takeMVar children
    putMVar children (mvar:childs)
    forkIO (io `finally` putMVar mvar ())
