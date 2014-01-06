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
{-# LANGUAGE TemplateHaskell #-}

{- |
    The CanvasHs.Protocol module handles the transition from internal CanvasHs Datamodel to the
    JSON data model and vice versa. It can encode 'RegularOutput' to a valid JSON string (using an UTF8 ByteString)
    with the encode function, and can decode incoming JSON messages (as UTF8 ByteStrings) to 
    'Event's using the decode function
-}
module CanvasHs.Protocol
(   encode
    ,decode
) where 

import CanvasHs.Data
import CanvasHs.Protocol.ShapeOutput
import CanvasHs.Protocol.ActionOutput
import CanvasHs.Protocol.Input

import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.Aeson as Aeson (encode, eitherDecode)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.UTF8 as BU (fromString, toString)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

-- | This datastructure represents a JSON Output string for Aeson
data JSONOutput = JSONOutput {
        shape :: Maybe JSONShape,
        actions :: [JSONAction]
        } deriving (Show)
        
-- | This templatehaskell will make JSONOutput derriving JSON, and will instruct Aeson to omit nothing fields
--   in the resulting JSON
$(deriveJSON defaultOptions{omitNothingFields=True} ''JSONOutput)

-- | encode encodes a RegularOutput to a JSON UTF8 ByteString
encode :: RegularOutput -> T.Text
encode o = T.pack $ BU.toString $ Aeson.encode (JSONOutput {shape=(shapeEncode <$> fst o), actions=(map actionEncode $ snd o)})

-- | decode decodes an incoming JSON UTF8 ByteString to an Event, 
--   handles the special case of the CanvasHs.Server "INIT" string (which indicates a connection has been established)
--   as a StartEvent
decode :: T.Text -> Event
decode "INIT"   = StartEvent
decode s        = either (\b -> error $ "Aeson decode error: "++b) (\b -> b) $ Aeson.eitherDecode $ BU.fromString $ T.unpack s
