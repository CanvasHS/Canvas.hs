{-# LANGUAGE OverloadedStrings #-}
module CanvasHs.Protocol.Input where


import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import Data.Text

import CanvasHs.Data

data JSONEvent = JSONEvent {
    event :: Text,
    eventData :: JSONEventData
} deriving(Eq, Show)

data JSONEventData = JSONEventData {
        jeventId :: Maybe Text,
        x :: Maybe Integer,
        y :: Maybe Integer,
        key :: Maybe Text,
        control :: Maybe Bool,
        alt :: Maybe Bool,
        shift :: Maybe Bool,
        meta :: Maybe Bool
    } deriving(Eq, Show)
    

instance FromJSON JSONEvent where
    parseJSON (Object v) = do
        name <- v .: "event"
        eventdata <- parseJSON =<< (v .: "data")

        {-result <- case name of 
            "mousedown" -> MouseDown (x $ eventdata, y $ eventdata) unpack $ jeventId $ eventdata
            "mouseclick" -> MouseClick (x $ eventdata, y $ eventdata) unpack $ jeventId $ eventdata
            "mouseup" -> MouseUp (x $ eventdata, y $ eventdata) unpack $ jeventId $ eventdata
            "mousedoubleclick" -> MouseDoubleClick (x $ eventdata, y $ eventdata) unpack $ id $ eventdata
            "mouseenter" -> MouseEnter (x $ eventdata, y $ eventdata) unpack $ jeventId $ eventdata
            "mouseleave" -> MouseLeave (x $ eventdata, y $ eventdata) unpack $ jeventId $ eventdata-}

        return $ JSONEvent name eventdata

instance FromJSON JSONEventData where
    parseJSON (Object v) = JSONEventData <$>
                            v .:? "id" <*>
                            v .:? "x" <*>
                            v .:? "y" <*>
                            v .:? "key" <*>
                            v .:? "control" <*>
                            v .:? "alt" <*>
                            v .:? "shift" <*>
                            v .:? "meta"

