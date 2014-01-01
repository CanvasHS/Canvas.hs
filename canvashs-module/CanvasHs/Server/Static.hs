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
    This module exposes functions to search for directories and files and serve those files
-}
module CanvasHs.Server.Static where

import Paths_canvashs 
import Data.Maybe
import Data.List
import Data.List.Split
import System.Directory

import qualified Network.Wai as WAI
import Network.HTTP.Types (status200)
import qualified Data.Text as T
import qualified Blaze.ByteString.Builder as BL (copyByteString)
import qualified Data.ByteString as BS

-- Files to be ignored
ignoreFiles :: [String]
ignoreFiles = ["..","."]

-- | builds WAI responses for requests for static files with a WAI response conatining the requested file 
--   and its content/type, if the file is in the provided files list 
httpget :: [([String], BS.ByteString)] -> WAI.Application
httpget files req = return $ do WAI.ResponseBuilder status200 [("Content-Type", encoding)] $ BL.copyByteString page
                    where
                        (encoding, page) | (WAI.pathInfo req) == [] = ("text/html", snd $ files !! 0)
                                         | file == Nothing = ("text/html", "Error 404")
                                         | hasExtension "css" = ("text/css", (fromJust file))
                                         | hasExtension "html" = ("text/html", (fromJust file))
                                         | hasExtension "jpg" = ("image/jpg", (fromJust file))
                                         | hasExtension "png" = ("image/png", (fromJust file))
                                         | hasExtension "js" = ("text/javascript", (fromJust file))
                                         | otherwise = ("", (fromJust file))
                        file = lookup request files
                        request = map T.unpack $ WAI.pathInfo req
                        hasExtension extension = (takeLast (length extension) $ last $ request) == extension
                        takeLast n xs = (reverse . (take n) . reverse) xs
                
-- | Gets all the directories in the given path
getDirectories :: String -> IO [FilePath]
getDirectories path = do
    dirPath <- getDataFileName path
    -- Find files and directories, filter out the ".." and "."
    filesAndDirectories <- getDirectoryContents dirPath >>= \x -> return (filter (not.(`elem` ignoreFiles)) x)
    directoryExists <- mapM doesDirectoryExist (map ((dirPath++"/")++) filesAndDirectories) --Check if directories exist
    return [ (path++"/")++(filesAndDirectories !! i) | i <- [0..(length filesAndDirectories-1)], directoryExists !! i ]

-- | Gets all files within the given dorectory
getDirectoryFiles :: String -> IO [([String], BS.ByteString)]
getDirectoryFiles path = do
    dirPath <- getDataFileName path
    filesAndDirectories <- getDirectoryContents dirPath >>= \x -> return (filter (not.(`elem` ignoreFiles)) x)
    filesExist <- mapM doesFileExist (map ((dirPath++"/")++) filesAndDirectories)
    existingFiles <- return $ [ (filesAndDirectories !! i) | i <- [0..(length filesAndDirectories-1)], filesExist !! i ]
    files <- mapM BS.readFile (map ((dirPath++"/")++) existingFiles)
    return $ zip (map ((delete "canvashs-client").(splitOn "/").((path++"/")++)) existingFiles) files
