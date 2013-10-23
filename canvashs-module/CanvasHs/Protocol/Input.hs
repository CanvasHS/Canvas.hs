{-# LANGUAGE OverloadedStrings #-}
module CanvasHs.Protocol.Input (FromJSON(..)) where


import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Data.Text
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

import CanvasHs.Data

data JSONEventData = JSONEventData {
        jeventId :: Maybe Text,
        x :: Maybe Integer,
        y :: Maybe Integer,
        key :: Maybe Text,
        control :: Maybe Bool,
        alt :: Maybe Bool,
        shift :: Maybe Bool,
        super :: Maybe Bool,
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
                            v .:? "super" <*>
                            v .:? "xdelta" <*>
                            v .:? "ydelta"

instance FromJSON Event where
    parseJSON (Object v) = do
        makeEvent <$>
            v .: "event" <*>
            v .: "eventData"

-- Ooit gehoord van pattern matching, nou ik blijkbaar wel
makeEvent :: Text -> JSONEventData -> Event
makeEvent "mousedown" 
    (JSONEventData{jeventId = Just id, x = Just x, y = Just y}) 
        = MouseDown (fromIntegral $ x, fromIntegral $ y) (unpack $ id)

makeEvent "mouseclick"
    (JSONEventData{jeventId = Just id, x = Just x, y = Just y})  
        = MouseClick (fromIntegral $ x, fromIntegral $ y) (unpack $ id)

makeEvent "mouseup" 
    (JSONEventData{jeventId = Just id, x = Just x, y = Just y})
         = MouseUp (fromIntegral $ x, fromIntegral $ y) (unpack $ id)

makeEvent "mousedoubleclick"
    (JSONEventData{jeventId = Just id, x = Just x, y = Just y})
         = MouseDoubleClick (fromIntegral $ x, fromIntegral $ y) (unpack $ id)

makeEvent "mouseenter"
    (JSONEventData{jeventId = Just id, x = Just x, y = Just y})
         = MouseEnter (fromIntegral $ x, fromIntegral $ y) (unpack $ id)

makeEvent "mouseleave"
    (JSONEventData{jeventId = Just id, x = Just x, y = Just y})
         = MouseLeave (fromIntegral $ x, fromIntegral $ y) (unpack $ id)

makeEvent "keydown"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh, super = Just su})
        = KeyDown ((unpack $ k) !! 0) (makeModifiers c a sh su)

makeEvent "keyup"
    (JSONEventData{key = Just k, control = Just c, alt = Just a, shift = Just sh, super = Just su})
        = KeyUp ((unpack $ k) !! 0) (makeModifiers c a sh su)

makeEvent "scroll"
    (JSONEventData{xdelta = Just x, ydelta = Just y})
        = Scroll (fromIntegral $ x) (fromIntegral $ y)

makeModifiers :: Bool -> Bool -> Bool -> Bool -> [Modifier]
makeModifiers ctrl alt shift super = 
    (if ctrl then [Ctrl] else []) ++ 
    (if alt then [Alt] else []) ++
    (if shift then [Shift] else []) ++
    (if super then [Super] else [])
