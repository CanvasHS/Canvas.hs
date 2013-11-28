{-# LANGUAGE OverloadedStrings #-}
module CanvasHs.Protocol.Input (FromJSON(..)) where


import Data.Aeson ((.:), (.:?), FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Data.Text

import CanvasHs.Data


data JSONEventData = JSONEventData {
        jeventId :: Maybe Text,
        x :: Maybe Integer,
        y :: Maybe Integer,
        x1 :: Maybe Integer, -- Only for mousedrag
        y1 :: Maybe Integer, -- Only for mousedrag
        jeventId1 :: Maybe Text, -- Only for mousedrag
        x2 :: Maybe Integer, -- Only for mousedrag
        y2 :: Maybe Integer, -- Only for mousedrag
        jeventId2 :: Maybe Text, -- Only for mousedrag
        key :: Maybe Text,
        control :: Maybe Bool,
        alt :: Maybe Bool,
        shift :: Maybe Bool,
        xdelta :: Maybe Integer,
        ydelta :: Maybe Integer,
        filename :: Maybe Text,
        filecontents :: Maybe Text
    } deriving(Eq, Show)

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
                            v .:? "id2"          <*> -- Only for mousedrag
                            v .:? "key"          <*>
                            v .:? "control"      <*>
                            v .:? "alt"          <*>
                            v .:? "shift"        <*>
                            v .:? "xdelta"       <*>
                            v .:? "ydelta"       <*>
                            v .:? "filename"     <*> -- Only for upload events
                            v .:? "filecontents"     -- Only for upload events
    parseJSON _ = error "A toplevel JSON should be an object"

instance FromJSON Event where
    parseJSON (Object v) = do
        makeEvent <$>
            v .: "event" <*>
            v .: "data"
    parseJSON _ = error "A toplevel JSON should be an object"

-- Ooit gehoord van pattern matching, nou ik blijkbaar wel
makeEvent :: Text -> JSONEventData -> Event
makeEvent "mousedown" 
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y}) 
        = MouseDown (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "mouseclick"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})  
        = MouseClick (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "mouseup" 
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseUp (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "mousedoubleclick"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseDoubleClick (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "mousedrag"
    (JSONEventData{jeventId1 = Just eid1, x1 = Just x1, y1 = Just y1, jeventId2 = Just eid2, x2 = Just x2, y2 = Just y2})
        = MouseDrag (fromIntegral $ x1, fromIntegral $ y1) (unpack $ eid1) (fromIntegral $ x2, fromIntegral $ y2) (unpack $ eid2)

makeEvent "mouseover"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseOver (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "mouseout"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseOut (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "keydown"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyDown (unpack $ k) (makeModifiers c a sh)

makeEvent "keyclick"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyClick (unpack $ k) (makeModifiers c a sh)

makeEvent "keyup"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyUp (unpack $ k) (makeModifiers c a sh)

makeEvent "scroll"
    (JSONEventData{xdelta = Just x, ydelta = Just y})
        = Scroll (fromIntegral $ x) (fromIntegral $ y)

makeEvent "upload"
    (JSONEventData{filename = Just fn, filecontents = Just fc})
        = Upload (unpack $ fn)  

makeEvent _ _ = error "JSON did not match any event"

makeModifiers :: Bool -> Bool -> Bool -> [Modifier]
makeModifiers ctrl alt shift = 
    (if ctrl then [Ctrl] else []) ++ 
    (if alt then [Alt] else []) ++
    (if shift then [Shift] else [])
