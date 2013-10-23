{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CanvasHs.Protocol.Output where

import GHC.Generics
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import Control.Applicative ((<$>))

import qualified CanvasHs.Data as D

data JSONShape
    = JSONShape { 
        shapeType      :: T.Text, 
        shapeData      :: JSONShapeData, 
        shapeEventData :: Maybe JSONEventData, 
        shapeChildren  :: Maybe [JSONShape]
    } deriving (Show, Generic)

data JSONShapeData
    = JSONShapeData { 
--        stroke         :: Maybe T.Text, 
        strokeWidth    :: Maybe Int, 
--        fill           :: Maybe T.Text, 
        scaleX         :: Maybe Float, 
        scaleY         :: Maybe Float, 
        rotateDeg      :: Maybe Int, 
        fontSize       :: Maybe Int, 
        fontFamily     :: Maybe T.Text, 
        points         :: Maybe [Int], 
        x              :: Maybe Int, 
        y              :: Maybe Int, 
        width          :: Maybe Int, 
        height         :: Maybe Int, 
        radius         :: Maybe Int 
    } deriving (Show, Generic)

data JSONEventData
    = JSONEventData { 
        eventId        :: Maybe T.Text, 
        listen         :: Maybe [T.Text]
    } deriving (Show, Generic)

instance ToJSON JSONShape
instance ToJSON JSONShapeData
instance ToJSON JSONEventData

-- | Interne encode, maakt van een Shape een JSONshape die dan naar Aeson kan
--    Let op, de primitieven (alles wat in CanvasHs.Data.Shape geen Shape als veld heeft)
--    Maken de daadwerkelijke JSONShape, alle andere Shapes passen deze hierdoor gebouwde
--     JSONShape's aan.
iEncode :: D.Shape -> JSONShape
iEncode (D.Rect p w h)        = JSONShape {shapeType = "rect" 
                                        ,shapeData = (iEncodePoint p) {width = Just w, height = Just h}
                                        ,shapeEventData = Nothing
                                        ,shapeChildren = Nothing
                                        }
iEncode (D.Circle p r)        = JSONShape {shapeType = "circle"
                                        ,shapeData = (iEncodePoint p) {radius = Just r}
                                        ,shapeEventData = Nothing
                                        ,shapeChildren = Nothing
                                        }
-- iEncode (Arc p r sa ea)    TODO: arc opnemen in het protocol!
iEncode (D.Line ps)            = JSONShape {shapeType = "line"
                                        ,shapeData = iEncodePoints ps
                                        ,shapeEventData = Nothing
                                        ,shapeChildren = Nothing
                                        }
iEncode (D.Polygon ps)        = JSONShape {shapeType = "polygon"
                                        ,shapeData = iEncodePoints ps
                                        ,shapeEventData = Nothing
                                        ,shapeChildren = Nothing
                                        }
-- iEncode (D.Text p s td) TODO: moeite
iEncode (D.Rotate deg s)        = js {shapeData = sd {rotateDeg = Just deg}}
                                where 
                                    js = iEncode s
                                    sd = shapeData js
iEncode (D.Translate dx dy s) = js {shapeData = sd {x = (+dx) <$> (x sd), y = (+dy) <$> (y sd)}}
                                where
                                    js = iEncode s
                                    sd = shapeData js
-- | TODO: Volgens protocol een scale :: Int, volgens datamodel een xscale en yscale
iEncode (D.Scale dx dy s)        = js {shapeData = sd {scaleX = Just dx, scaleY = Just dy}}
                                where 
                                    js = iEncode s
                                    sd = shapeData js
iEncode (D.Event e s)            = js {shapeEventData = Just (iEncodeEventData (shapeEventData js) e)}
                                where
                                    js = iEncode s
-- | TODO: moet een container niet ook een point?
iEncode (D.Container w h ss)    = JSONShape {shapeType = "container"
                                        ,shapeData = iEncodePoint (0,0)
                                        ,shapeEventData = Nothing
                                        ,shapeChildren = Just $ map iEncode ss
                                        }

iEncodePoint :: D.Point -> JSONShapeData
iEncodePoint (x',y')    
    = JSONShapeData { 
--        stroke         = Nothing,
        strokeWidth    = Nothing, 
--        fill           = Nothing, 
        scaleX         = Nothing, 
        scaleY         = Nothing, 
        rotateDeg      = Nothing, 
        fontSize       = Nothing, 
        fontFamily     = Nothing, 
        points         = Nothing,
        x              = Just x', 
        y              = Just y', 
        width          = Nothing, 
        height         = Nothing, 
        radius         = Nothing 
    }
    
iEncodePoints :: [D.Point] -> JSONShapeData
iEncodePoints ps
    = JSONShapeData { 
--        stroke         = Nothing,
        strokeWidth    = Nothing, 
--        fill           = Nothing, 
        scaleX         = Nothing, 
        scaleY         = Nothing, 
        rotateDeg      = Nothing, 
        fontSize       = Nothing, 
        fontFamily     = Nothing, 
        points         = Just $ eps [] $ reverse ps,
        x              = Nothing, 
        y              = Nothing, 
        width          = Nothing, 
        height         = Nothing, 
        radius         = Nothing 
    }
        where 
            eps a []              = a
            eps a ((x',y'):ps)    = eps (x':y':a) ps

iEncodeEventData :: Maybe JSONEventData -> D.EventData -> JSONEventData
iEncodeEventData Nothing e     = iEncodeEventData (Just (JSONEventData{eventId = Nothing, listen = Just []})) e
iEncodeEventData (Just j) e = j {eventId = Just $ T.pack $ D.eventId e
                                ,listen = (++ mklisten e) <$> listen j
                                }
                                where
                                    -- | TODO: protocol spreekt van camelCase (mouseDown) terwijl voorbeeldcode van mousedown spreekt
                                    mklisten e =     [] 
                                                    ++ if D.mouseDown e then ["mousedown"] else []
                                                    ++ if D.mouseClick e then ["mouseclick"] else []
                                                    ++ if D.mouseUp e then ["mouseup"] else []
                                                    ++ if D.mouseDoubleClick e then ["mousedubbleclick"] else []
                                                    ++ if D.mouseDrag e then ["mousedrag"] else []
                                                    ++ if D.mouseEnter e then ["mouseenter"] else []
                                                    ++ if D.mouseLeave e then ["mouseleave"] else []













