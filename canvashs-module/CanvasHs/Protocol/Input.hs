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
        key :: Maybe Text,
        control :: Maybe Bool,
        alt :: Maybe Bool,
        shift :: Maybe Bool,
        xdelta :: Maybe Integer,
        ydelta :: Maybe Integer
    } deriving(Eq, Show)

instance FromJSON JSONEventData where
    parseJSON (Object v) = JSONEventData <$>
                            v .:? "id" <*>
                            v .:? "x" <*>
                            v .:? "y" <*>
                            v .:? "key" <*>
                            v .:? "control" <*>
                            v .:? "alt" <*>
                            v .:? "shift" <*>
                            v .:? "xdelta" <*>
                            v .:? "ydelta"
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

makeEvent "mouseover"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseOver (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "mouseout"
    (JSONEventData{jeventId = Just eid, x = Just x, y = Just y})
         = MouseOut (fromIntegral $ x, fromIntegral $ y) (unpack $ eid)

makeEvent "keydown"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyDown ((unpack $ k) !! 0) (makeModifiers c a sh)

makeEvent "keyclick"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyClick ((unpack $ k) !! 0) (makeModifiers c a sh)

makeEvent "keyup"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh})
        = KeyUp ((unpack $ k) !! 0) (makeModifiers c a sh)

makeEvent "scroll"
    (JSONEventData{xdelta = Just x, ydelta = Just y})
        = Scroll (fromIntegral $ x) (fromIntegral $ y)

makeEvent _ _ = error "JSON did not match any event"

makeModifiers :: Bool -> Bool -> Bool -> [Modifier]
makeModifiers ctrl alt shift = 
    (if ctrl then [Ctrl] else []) ++ 
    (if alt then [Alt] else []) ++
    (if shift then [Shift] else [])
