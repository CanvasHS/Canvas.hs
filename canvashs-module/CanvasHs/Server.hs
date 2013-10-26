{-# LANGUAGE OverloadedStrings #-}
module CanvasHs.Server (start) where

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
import System.IO.Unsafe

import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Control.Monad.Trans (liftIO, lift)


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
                waitForChildren -- wait until threads finish, this allows the process to be killed
                return ()
                where
                    serverHandle = liftIO $ WS.runServer "0.0.0.0" 8080 $ websockets f
                

serverHttp :: IO ()
serverHttp = do
                -- Serve static files
                dirFiles <- getDirectories "canvashs-client" >>= \dirs -> mapM getDirectoryFiles ("canvashs-client":dirs)
                forkIO $ WRP.run 8000 (httpget (concat dirFiles))
                return ()

        
--  WEBSOCKETS          
-- RFC6455 heeft de beste browsersupport, zie ook: http://en.wikipedia.org/wiki/WebSocket#Browser_support
-- zit om een vage reden niet in SebSockets (ondanks dat doc zegt van wel), Hybi00 werkt iig in IE11 en Chrome 29
websockets :: (T.Text -> IO (Maybe T.Text)) -> WS.Request -> WS.WebSockets WS.Hybi00 ()
websockets f rq =   do
                    WS.acceptRequest rq
                    (Just initial) <- liftIO $ f $ T.pack "INIT" -- StartState
                    WS.sendTextData initial -- send the initial frame generated by the callback
                    forever $ do
                        msg <- WS.receiveData
                        resp <- liftIO $ f msg
                        if (isNothing resp)
                            then return ()
                            else do 
                                let 
                                    Just m = resp
                                WS.sendTextData m       
                        
                        
-- Used to manage threads
children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

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