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

module FileUtils where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Base64 as B64
import Control.Monad.Trans (liftIO, lift)

getFileLines :: FilePath -> IO [String]
getFileLines path = do
                        contents <- readFile path
                        return (lines contents)

saveBase64File :: T.Text -> IO()
saveBase64File b64 = do
                        case (B64.decode $ B.fromString $ T.unpack $ b64) of
                            (Left a) -> putStrLn a
                            (Right b) -> putStrLn $ B.toString b
