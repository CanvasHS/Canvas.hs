module CanvasHs.Launch (launchBrowser) where

import System.Info
import System.Process
import System.Exit

launchBrowser :: String -> IO()
launchBrowser url = do
                status <- case os of 
                    "windows" -> launchBrowserWindows url
                    "mingw32" -> launchBrowserWindows url
                    "osx"     -> launchBrowserOSX url
                    _         -> launchBrowserLinux url
                case status of
                    ExitSuccess   -> return ()
                    ExitFailure n -> error ("Failed to open browser, exitcode " ++ show n)

launchBrowserWindows :: String -> IO(ExitCode)
launchBrowserWindows url = do
                status <- system $ "start " ++ url
                return status

launchBrowserLinux :: String -> IO(ExitCode)
launchBrowserLinux url = do
                status <- system $ "xdg-open " ++ url
                return status

launchBrowserOSX :: String -> IO(ExitCode)
launchBrowserOSX url = do
                status <- system $ "open " ++ url
                return status
