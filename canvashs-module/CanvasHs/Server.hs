{-# LANGUAGE OverloadedStrings #-}
module CanvasHs.Server (start) where

import CanvasHs.Protocol
import CanvasHs.Data

-- Paths_canvashs is required to include static files
import Paths_canvashs 
import qualified Network.WebSockets as WS
import Control.Monad (forever)
import qualified Data.Text as T
import Data.Maybe

import qualified Network.Wai as WAI
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as WRP (run)
import qualified Blaze.ByteString.Builder as BL (copyByteString)
import Data.Monoid
import qualified Data.ByteString.UTF8 as BU
import Control.Concurrent (forkIO)
import System.Directory

import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Control.Monad.Trans (liftIO, lift)
import Debug.Trace


start :: IO ()
start = do
        forkIO serverHttp --de httpserver draait in een apart thread
        serverHandle --runserver is een extreem simpele server voor de websockets   
        return ()
        where
            serverHandle = liftIO $ WS.runServer "0.0.0.0" 8080 $ websockets

serverHttp :: IO ()
serverHttp = do
                staticContent <- getDataFileName "canvashs-client/index.html" >>= readFile
                dirFiles <- (getDirectoryFiles "canvashs-client")
                files <- mapM getFile ["index.html", "js/canvashs.js", "js/jquery.js", "js/kinetic.js"]
                traceShow dirFiles $ forkIO $ WRP.run 8000 (httpget (files))
                return ()

getDirectoryFiles :: String -> IO [String]
getDirectoryFiles path = do
    files <- getDataFileName path >>= getDirectoryContents 
    filesExist <- mapM doesFileExist files
    mapM getFile [ files !! i | i <- [0..(length files-1)], filesExist !! i ]
getFile :: String -> IO String
getFile name = getDataFileName ("canvashs-client/" ++ name) >>=readFile
--getFileMaybe :: String -> Maybe (IO String)
--getFileMaybe name | doesFileExist path = Just (getDataFileName path >>=readFile)
--                  | otherwise = Nothing
--    where
--        path = ("canvashs-client/" ++ name)
--  HTTP GET
httpget :: [String] -> WAI.Application
httpget a req = traceShow (WAI.pathInfo req) $ return $ do WAI.ResponseBuilder status200 [("Content-Type", encoding)] $ BL.copyByteString $ BU.fromString page
                    where
                        (encoding, page) = case WAI.pathInfo req of
                                    ["js","jquery.js"] -> ("text/javascript", (a !! 3))
                                    ["js","kinetic.js"] -> ("text/javascript", (a !! 2))
                                    ["js","canvashs.js"] -> ("text/javascript", (a !! 1))
                                    _ -> ("text/html", (a !! 0))
                
        
--  WEBSOCKETS          
-- RFC6455 heeft de beste browsersupport, zie ook: http://en.wikipedia.org/wiki/WebSocket#Browser_support
-- zit om een vage reden niet in SebSockets (ondanks dat doc zegt van wel), Hybi00 werkt iig in IE11 en Chrome 29
websockets :: WS.Request -> WS.WebSockets WS.Hybi00 ()
websockets rq = do
                        WS.acceptRequest rq
                        WS.sendTextData $ encode (Circle (1,3) 100)
