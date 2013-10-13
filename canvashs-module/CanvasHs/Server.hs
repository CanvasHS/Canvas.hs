{-# LANGUAGE OverloadedStrings #-}
module CanvasHs.Server (start) where

import CanvasHs.Protocol



import qualified Network.WebSockets as WS
import Control.Monad (forever)
import qualified Data.Text as T

import qualified Network.Wai as WAI
import Network.HTTP.Types (status200)
import qualified Network.Wai.Handler.Warp as WRP (run)
import qualified Blaze.ByteString.Builder as BL (copyByteString)
import Data.Monoid
import qualified Data.ByteString.UTF8 as BU
import Control.Concurrent (forkIO)

import Data.IORef (IORef, newIORef, atomicModifyIORef)
import Control.Monad.Trans (liftIO, lift)

data CData = CI Int 

start :: IO ()
start = do
        forkIO serverHttp --de httpserver draait in een apart thread
        counter <- newIORef (CI 0)
        serverHandle <- WS.runServer "0.0.0.0" 8080 $ websockets counter --runserver is een extreem simpele server voor de websockets   
        return ()

serverHttp :: IO ()
serverHttp = do
                staticContent <- readFile "websocketstest.html"
                WRP.run 8000 (httpget staticContent)
                return ()

--  HTTP GET
httpget :: String -> WAI.Application
httpget a req = return $ do 
                    WAI.ResponseBuilder status200 [("Content-Type", "text/html")] $ BL.copyByteString $ BU.fromString a
        
--  WEBSOCKETS          
-- RFC6455 heeft de beste browsersupport, zie ook: http://en.wikipedia.org/wiki/WebSocket#Browser_support
-- zit om een vage reden niet in SebSockets (ondanks dat doc zegt van wel), Hybi00 werkt iig in IE11 en Chrome 29
websockets :: IORef CData -> WS.Request -> WS.WebSockets WS.Hybi00 ()
websockets counter rq = do
                        WS.acceptRequest rq
                        WS.sendTextData $ T.pack "Hallo client, wat wil je met 3 vermenigvuldigen?"
                        vermenigvuldig counter
                    
vermenigvuldig :: IORef CData -> WS.WebSockets WS.Hybi00 ()
vermenigvuldig counter = forever $ do
                            msg <- WS.receiveData
                            newC <- liftIO $ incCounter counter
                            WS.sendTextData $ T.concat [resultaat $ readMsg msg, T.pack (" | De counter is: " ++ (show $ readCData newC))]
                            where 
                                readMsg :: T.Text -> Maybe Int
                                readMsg m = let rs = (reads $ T.unpack m)::[(Int, String)] in
                                            if rs /= [] then
                                                Just $ fst $ head rs
                                            else
                                                Nothing
                    
resultaat :: Maybe Int -> T.Text
resultaat Nothing  = "Dat is geen leesbaar getal, grapjas"
resultaat (Just 0) = "0 keer iets is 0, dat weet toch iedereen mallerd"
resultaat (Just x) = T.pack $ ("Je resultaat is: " ++ (show $ x*3))

incCounter :: IORef CData -> IO CData
incCounter c = atomicModifyIORef c (\ct -> ((CI ((readCData ct) +1)), (CI (readCData ct))))
                
readCData :: CData -> Int
readCData (CI n) = n