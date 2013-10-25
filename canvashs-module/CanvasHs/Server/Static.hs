{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.UTF8 as BU

-- Files to be ignored
ignoreFiles :: [String]
ignoreFiles = ["..","."]

-- Serve static files
httpget :: [([String], String)] -> WAI.Application
httpget files req = return $ do WAI.ResponseBuilder status200 [("Content-Type", encoding)] $ BL.copyByteString $ BU.fromString page
                    where
                        (encoding, page) | (WAI.pathInfo req) == [] = ("text/html", snd $ files !! 0)
                                         | file == Nothing = ("text/html", "Error 404")
                                         | otherwise = ("", (fromJust file))
                        file = lookup (map T.unpack $ WAI.pathInfo req) files
                
-- Get directory names in a directory path
getDirectories :: String -> IO [FilePath]
getDirectories path = do
    dirPath <- getDataFileName path
    -- Find files and directories, filter out the ".." and "."
    filesAndDirectories <- getDirectoryContents dirPath >>= \x -> return (filter (not.(`elem` ignoreFiles)) x)
    directoryExists <- mapM doesDirectoryExist (map ((dirPath++"/")++) filesAndDirectories) --Check if directories exist
    return [ (path++"/")++(filesAndDirectories !! i) | i <- [0..(length filesAndDirectories-1)], directoryExists !! i ]

-- Get files in a directory
getDirectoryFiles :: String -> IO [([String], String)]
getDirectoryFiles path = do
    dirPath <- getDataFileName path
    filesAndDirectories <- getDirectoryContents dirPath >>= \x -> return (filter (not.(`elem` ignoreFiles)) x)
    filesExist <- mapM doesFileExist (map ((dirPath++"/")++) filesAndDirectories)
    existingFiles <- return $ [ (filesAndDirectories !! i) | i <- [0..(length filesAndDirectories-1)], filesExist !! i ]
    files <- mapM readFile (map ((dirPath++"/")++) existingFiles)
    return $ zip (map ((delete "canvashs-client").(splitOn "/").((path++"/")++)) existingFiles) files
