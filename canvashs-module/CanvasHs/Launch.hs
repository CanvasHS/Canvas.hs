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
    This module exposes a function which will launch a browser pointing to
    a given url on Windows (32 and 64 bits), OSX and multiple UNIX variants.
    The launched browser will be the users default browser
-}
module CanvasHs.Launch (launchBrowser) where

import Prelude
import System.Info
import System.Process
import System.Exit

-- | Opens a new browser window or tab (depending on the users settings) pointing to the given url 
--   in the system default browser.
launchBrowser :: String -> IO()
launchBrowser url = do
                status <- case os of 
                    "windows" -> launchBrowserWindows url
                    "mingw32" -> launchBrowserWindows url
                    "darwin"  -> launchBrowserOSX url
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
