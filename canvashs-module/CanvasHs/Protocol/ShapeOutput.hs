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

{-
    The CanvasHs.Protocol.ShapeOutput module exposes a function and a datatype used in encoding 
    CanvasHs Data to JSON. It will encode the 'Shape' to a JSONShape which derrives JSON so
    it can be encoded by Aeson
-}
module CanvasHs.Protocol.ShapeOutput
(   shapeEncode
,   JSONShape
)   where

import GHC.Generics
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Aeson.TH
import qualified Data.Text as T
import Control.Applicative ((<$>))

import qualified CanvasHs.Data as D

-- | JSONShape represents a 'Shape' as an object which can ve encoded by Aeson
data JSONShape
    = JSONShape { 
        -- Keep these exactly this way, 'shape' is dropped in the ToJSON instance
        shapetype      :: T.Text,
        shapedata      :: JSONShapeData, 
        shapeeventData :: Maybe JSONEventData, 
        shapechildren  :: Maybe [JSONShape]
    } deriving (Show)

-- | JSONShapeData is part of the JSONShape and represents the ShapeData as an object which can be encoded by Aeson    
data JSONShapeData
    = JSONShapeData { 
        stroke         :: Maybe JSONRGBAColor, 
        strokeWidth    :: Maybe Int, 
        fill           :: Maybe JSONRGBAColor, 
        scaleX         :: Maybe Float, 
        scaleY         :: Maybe Float, 
        rotationDeg    :: Maybe Int, 
        fontSize       :: Maybe Int, 
        fontFamily     :: Maybe T.Text,
        text           :: Maybe T.Text,
        align          :: Maybe T.Text,
        bold           :: Maybe Bool,
        italic         :: Maybe Bool,
        underline      :: Maybe Bool,
        points         :: Maybe [Int],
        offset         :: Maybe [Int],
        x              :: Maybe Int, 
        y              :: Maybe Int,
        width          :: Maybe Int, 
        height         :: Maybe Int, 
        radius         :: Maybe Int 
    } deriving (Show)

-- | JSONEventData is part of the JSONShape and represents EventData as an object which can be encodede by Aeson    
data JSONEventData
    = JSONEventData { 
        eventId        :: Maybe T.Text, 
        listen         :: Maybe [T.Text]
    } deriving (Show)

-- | JSONRGBAColor is part of JSONShapeData and represents a RGBAColor as an object which can be encoded by AEson
data JSONRGBAColor
    = JSONRGBAColor {
        colr :: Int,
        colg :: Int,
        colb :: Int,
        cola :: Float
    } deriving (Show)

-- | This templateHaskell will make JSONShape derrive JSON, and will drop 'shape' 
--   from and omit empty fields from the resulting JSON    
$(deriveJSON defaultOptions{omitNothingFields=True, fieldLabelModifier = drop 5} ''JSONShape)

-- | This templateHaskell will make JSONShapeData derrive JSON, and will omit empty fields from the resulting JSON   
$(deriveJSON defaultOptions{omitNothingFields=True} ''JSONShapeData)

-- | This templateHaskell will make JSONEventData derrive JSON, and will omit empty fields from the resulting JSON   
$(deriveJSON defaultOptions{omitNothingFields=True} ''JSONEventData)

-- | This templateHaskell will make JSONShape derrive JSON, and will drop 'shape' from the resulting JSON
$(deriveJSON defaultOptions{omitNothingFields=True, fieldLabelModifier = drop 3} ''JSONRGBAColor)

-- | Converts a 'Shape' to a 'JSONShape' which can be encoded by Aeson
--   Note how only the primitives will result in an actual JSONShape, all others will
--   merely edit said shape
shapeEncode :: D.Shape -> JSONShape
shapeEncode (D.Rect p w h)          = JSONShape {shapetype = "rect" 
                                            ,shapedata = (shapeEncodePoint p) {width = Just w, height = Just h}
                                            ,shapeeventData = Nothing
                                            ,shapechildren = Nothing
                                            }
shapeEncode (D.Circle p r)          = JSONShape {shapetype = "circle"
                                            ,shapedata = (shapeEncodePoint p) {radius = Just r}
                                            ,shapeeventData = Nothing
                                            ,shapechildren = Nothing
                                            }
-- shapeEncode (Arc p r sa ea)    TODO: arc opnemen in het protocol!
shapeEncode (D.Line ps)             = JSONShape {shapetype = "line"
                                            ,shapedata = shapeEncodePoints ps
                                            ,shapeeventData = Nothing
                                            ,shapechildren = Nothing
                                            }
shapeEncode (D.Polygon ps)          = JSONShape {shapetype = "polygon"
                                            ,shapedata = shapeEncodePoints ps
                                            ,shapeeventData = Nothing
                                            ,shapechildren = Nothing
                                            }
shapeEncode (D.Text p s td)         = JSONShape { shapetype = "text"
                                            ,shapedata = shapeEncodeTextData p s td
                                            ,shapeeventData = Nothing
                                            ,shapechildren = Nothing
                                            }

shapeEncode (D.Fill (r,g,b,a) s)    = updateSD $ recurFill $ shapeEncode s
                                where
                                    recurFill :: JSONShape -> JSONShape
                                    recurFill js = js{shapechildren = (map updateSD) <$> shapechildren js}
                                    updateSD :: JSONShape -> JSONShape --could be in lambda but that decreases readability
                                    updateSD js = recurFill $ js{shapedata = (shapedata js){fill = Just JSONRGBAColor{colr=r, colg=g, colb=b, cola=a}}}  

shapeEncode (D.Stroke (r,g,b,a) w s)= updateSD $ recurStroke $ shapeEncode s
                                where
                                    recurStroke :: JSONShape -> JSONShape
                                    recurStroke js = js{shapechildren = (map updateSD) <$> shapechildren js}
                                    updateSD :: JSONShape -> JSONShape --could be in lambda but that decreases readability
                                    updateSD js = recurStroke $ js{shapedata = (shapedata js){stroke = Just JSONRGBAColor{colr=r, colg=g, colb=b, cola=a}, strokeWidth = Just w}}                                    

shapeEncode (D.Rotate deg s)        = js {shapedata = sd {rotationDeg = Just deg}}
                                where 
                                    js = shapeEncode s
                                    sd = shapedata js
shapeEncode (D.Translate dx dy s)   = js {shapedata = sd {x = (+dx) <$> (x sd), y = (+dy) <$> (y sd)}}
                                where
                                    js = shapeEncode s
                                    sd = shapedata js

shapeEncode (D.Scale dx dy s)       = js {shapedata = sd {scaleX = Just dx, scaleY = Just dy}}
                                where 
                                    js = shapeEncode s
                                    sd = shapedata js

shapeEncode (D.Event e s)           = js {shapeeventData = Just (shapeEncodeEventData (shapeeventData js) e)}
                                where
                                    js = shapeEncode s

shapeEncode (D.Offset (x,y) s)      = js {shapedata = sd {offset = Just [x,y]}}
                                where
                                    js = shapeEncode s
                                    sd = shapedata js

shapeEncode (D.Container w h ss)    = JSONShape {shapetype = "container"
                                            ,shapedata = (shapeEncodePoint (0,0)) {width = Just w, height = Just h}
                                            ,shapeeventData = Nothing
                                            ,shapechildren = Just $ map shapeEncode ss
                                            }

-- | a helper function which creates an JSONShapeData holding only a point and the default black fill
shapeEncodePoint :: D.Point -> JSONShapeData
shapeEncodePoint (x',y')    
    = JSONShapeData { 
        stroke         = Nothing,
        strokeWidth    = Nothing, 
        fill           = Just JSONRGBAColor{colr=0, colg=0, colb=0, cola = 1.0}, 
        scaleX         = Nothing, 
        scaleY         = Nothing, 
        rotationDeg    = Nothing, 
        fontSize       = Nothing, 
        fontFamily     = Nothing,
        text           = Nothing,
        align          = Nothing,
        bold           = Nothing,
        italic         = Nothing,
        underline      = Nothing,
        points         = Nothing,
        offset         = Nothing,
        x              = Just x', 
        y              = Just y', 
        width          = Nothing, 
        height         = Nothing, 
        radius         = Nothing 
    }

-- | a helper function which creates an JSONShapeData holding only a list of points and the default black fill
shapeEncodePoints :: [D.Point] -> JSONShapeData
shapeEncodePoints pts
    = JSONShapeData { 
        stroke         = Nothing,
        strokeWidth    = Nothing, 
        fill           = Just JSONRGBAColor{colr=0, colg=0, colb=0, cola = 1.0}, 
        scaleX         = Nothing, 
        scaleY         = Nothing, 
        rotationDeg    = Nothing, 
        fontSize       = Nothing, 
        fontFamily     = Nothing, 
        text           = Nothing,
        align          = Nothing,
        bold           = Nothing,
        italic         = Nothing,
        underline      = Nothing,
        points         = Just $ eps pts,
        offset         = Nothing,
        x              = Nothing, 
        y              = Nothing, 
        width          = Nothing, 
        height         = Nothing, 
        radius         = Nothing 
    }
        where
            -- Deze functie zet alle punten achter elkaar
            eps :: [(Int, Int)] -> [Int]
            eps []           = []        
            eps ((x',y'):ps) = x':y':(eps ps)

-- | a helper function which creates an JSONShapeData holding the given point, string to be drawn and textdata
shapeEncodeTextData :: D.Point -> String -> D.TextData -> JSONShapeData
shapeEncodeTextData ps s (D.TextData{D.font = f, D.size = si, D.alignment = a, D.bold = b, D.italic = i, D.underline = u}) = result
        where
            pointData = shapeEncodePoint ps
            al = case a of 
                        D.AlignLeft -> Just "left"
                        D.AlignCenter -> Just "center"
                        D.AlignRight -> Just "right"

            text = pointData{text= Just $ T.pack $ s}
            result = text{fontFamily=Just $ T.pack f, fontSize=Just si, align =al, bold=Just b, italic=Just i, underline=Just u}


-- | a helper function which encodes EventData into JSONEcentData, 
--   it can either update an existing JSONEventData (Just e) or create a new one (Nothing)
shapeEncodeEventData :: Maybe JSONEventData -> D.EventData -> JSONEventData
shapeEncodeEventData Nothing e     = shapeEncodeEventData (Just (JSONEventData{eventId = Nothing, listen = Just []})) e
shapeEncodeEventData (Just j) e = j {eventId = Just $ T.pack $ D.eventId e
                                ,listen = (++ mklisten e) <$> listen j
                                }
                                where
                                    mklisten e =     [] 
                                                    ++ (if D.mouseDown e then ["mousedown"] else [])
                                                    ++ (if D.mouseClick e then ["mouseclick"] else [])
                                                    ++ (if D.mouseUp e then ["mouseup"] else [])
                                                    ++ (if D.mouseDoubleClick e then ["mousedoubleclick"] else [])
                                                    ++ (if D.mouseDrag e then ["mousedrag"] else [])
                                                    ++ (if D.mouseOver e then ["mouseover"] else [])
                                                    ++ (if D.mouseOut e then ["mouseout"] else [])
                                                    ++ (if D.scroll e then ["scroll"] else [])
