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
    The CanvasHs.Protocol.Input module exposes a FromJSON instance for 'Event' which allows
    JOSN strings describing an event te bo decoded by Aeson
-}
module CanvasHs.Protocol.Input (FromJSON(..)) where


import Data.Aeson ((.:), (.:?), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import System.FilePath.Posix (takeExtension)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.ByteString.Base64.Lazy as B64

import CanvasHs.Data

-- | JSONEventData describes eventdata which could be incoming in a JSONstring as a record which can later be used
--   to construct an 'Event'
data JSONEventData = JSONEventData {
        jeventId :: Maybe BU.ByteString,
        x :: Maybe Int,
        y :: Maybe Int,
        x1 :: Maybe Int, -- Only for mousedrag
        y1 :: Maybe Int, -- Only for mousedrag
        jeventId1 :: Maybe BU.ByteString, -- Only for mousedrag
        x2 :: Maybe Int, -- Only for mousedrag
        y2 :: Maybe Int, -- Only for mousedrag
        key :: Maybe BU.ByteString,
        control :: Maybe Bool,
        alt :: Maybe Bool,
        shift :: Maybe Bool,
        xdelta :: Maybe Int,
        ydelta :: Maybe Int,
        width :: Maybe Int,
        height :: Maybe Int,
        filename :: Maybe BSL.ByteString,
        filecontents :: Maybe BSL.ByteString,
        value :: Maybe BSL.ByteString
    } deriving(Eq, Show)

-- | The FromJSON instance for JSONEventData allows the fromJSON instance of 'Event' to use parseJSON on the data field    
instance FromJSON JSONEventData where
    parseJSON (Object v) = JSONEventData         <$>
                            v .:? "id"           <*>
                            v .:? "x"            <*>
                            v .:? "y"            <*>
                            v .:? "x1"           <*> -- Only for mousedrag
                            v .:? "y1"           <*> -- Only for mousedrag
                            v .:? "id1"          <*> -- Only for mousedrag
                            v .:? "x2"           <*> -- Only for mousedrag
                            v .:? "y2"           <*> -- Only for mousedrag
                            v .:? "key"          <*>
                            v .:? "control"      <*>
                            v .:? "alt"          <*>
                            v .:? "shift"        <*>
                            v .:? "xdelta"       <*>
                            v .:? "ydelta"       <*>
                            v .:? "width"        <*>
                            v .:? "height"       <*>
                            v .:? "filename"     <*> -- Only for upload events
                            v .:? "filecontents" <*> -- Only for upload events
                            v .:? "value"
    parseJSON _ = error "A toplevel JSON should be an object"

-- | The FromJSON instance of 'Event' allows incoming JSON strings describing an event to be decoded by Aeson, 
--   incoming strings hold an event field identyfing the type of event and a datafield which describes the event
--   both of these are read by Aeson and read by the makeEvent function
instance FromJSON Event where
    parseJSON (Object v) = do
        makeEvent <$>
            v .: "event" <*>
            v .: "data"
    parseJSON _ = error "A toplevel JSON should be an object"

-- Ooit gehoord van pattern matching, nou ik blijkbaar wel
makeEvent :: BU.ByteString -> JSONEventData -> Event
makeEvent "mousedown" 
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y}) 
        = MouseDown (x, y) (BU.toString eid)

makeEvent "mouseclick"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})  
        = MouseClick (x, y) (BU.toString eid)

makeEvent "mouseup" 
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseUp (x, y) (BU.toString eid)

makeEvent "mousedoubleclick"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseDoubleClick (x, y) (BU.toString eid)

makeEvent "mousedrag"
    (JSONEventData{jeventId1 = Just eid1, x1 = Just x1, y1 = Just y1, x2 = Just x2, y2 = Just y2})
        = MouseDrag (x1, y1) (BU.toString eid1) (x2, y2)

makeEvent "mouseover"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseOver (x, y) (BU.toString eid)

makeEvent "mouseout"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseOut (x, y) (BU.toString eid)

makeEvent "keydown"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyDown (BU.toString k) (makeModifiers c a sh)

makeEvent "keyclick"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyClick (BU.toString k) (makeModifiers c a sh)

makeEvent "keyup"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyUp (BU.toString k) (makeModifiers c a sh)

makeEvent "scroll"
    (JSONEventData{jeventId = Just eid, xdelta = Just xd, ydelta = Just yd})
        = Scroll (BU.toString eid) xd yd

makeEvent "upload"
    (JSONEventData{filecontents = Just fc})
        = UploadComplete (BUL.toString $ b, b)
        where
            (Right b) = B64.decode fc
            
makeEvent "resizewindow"
    (JSONEventData{width = Just w, height = Just h})
        = WindowResize w h

makeEvent "prompt"
    (JSONEventData{value = Just val})
        = PromptResponse (BUL.toString val)

makeEvent _ _ = error "JSON did not match any event"

-- | a helper function to make a modifierlist from the incoming JSON
makeModifiers :: Bool -> Bool -> Bool -> [Modifier]
makeModifiers ctrl alt shift = 
    (if ctrl then [Ctrl] else []) ++ 
    (if alt then [Alt] else []) ++
    (if shift then [Shift] else [])
