{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

data JSONShape
    = JSONShape { 
        -- Keep these exactly this way, 'shape' is dropped in the ToJSON instance
        shapetype      :: T.Text,
        shapedata      :: JSONShapeData, 
        shapeeventData :: Maybe JSONEventData, 
        shapechildren  :: Maybe [JSONShape]
    } deriving (Show)

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
        points         :: Maybe [Int],
        offset         :: Maybe [Int],
        x              :: Maybe Int, 
        y              :: Maybe Int,
        width          :: Maybe Int, 
        height         :: Maybe Int, 
        radius         :: Maybe Int 
    } deriving (Show)

data JSONEventData
    = JSONEventData { 
        eventId        :: Maybe T.Text, 
        listen         :: Maybe [T.Text]
    } deriving (Show)

data JSONRGBAColor
    = JSONRGBAColor {
        colr :: Int,
        colg :: Int,
        colb :: Int,
        cola :: Float
    } deriving (Show)

$(deriveJSON defaultOptions{omitNothingFields=True, fieldLabelModifier = drop 5} ''JSONShape)

$(deriveJSON defaultOptions{omitNothingFields=True} ''JSONShapeData)

$(deriveJSON defaultOptions{omitNothingFields=True} ''JSONEventData)

$(deriveJSON defaultOptions{omitNothingFields=True, fieldLabelModifier = drop 3} ''JSONRGBAColor)

-- | Interne encode, maakt van een Shape een JSONshape die dan naar Aeson kan
--    Let op, de primitieven (alles wat in CanvasHs.Data.Shape geen Shape als veld heeft)
--    Maken de daadwerkelijke JSONShape, alle andere Shapes passen deze hierdoor gebouwde
--     JSONShape's aan.
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

shapeEncode (D.Offset x y s)        = js {shapedata = sd {offset = Just [x,y]}}
                                where
                                    js = shapeEncode s
                                    sd = shapedata js

shapeEncode (D.Container w h ss)    = JSONShape {shapetype = "container"
                                            ,shapedata = (shapeEncodePoint (0,0)) {width = Just w, height = Just h}
                                            ,shapeeventData = Nothing
                                            ,shapechildren = Just $ map shapeEncode ss
                                            }

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
        points         = Nothing,
        offset         = Nothing,
        x              = Just x', 
        y              = Just y', 
        width          = Nothing, 
        height         = Nothing, 
        radius         = Nothing 
    }
    
shapeEncodePoints :: [D.Point] -> JSONShapeData
shapeEncodePoints ps
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
        points         = Just $ eps [] $ reverse ps,
        offset         = Nothing,
        x              = Nothing, 
        y              = Nothing, 
        width          = Nothing, 
        height         = Nothing, 
        radius         = Nothing 
    }
        where
            -- Deze functie zet alle punten achter elkaar
            eps :: [Int] -> [(Int, Int)] -> [Int]
            eps a []              = a
            eps a ((x',y'):ps)    = eps (x':y':a) ps

shapeEncodeTextData :: D.Point -> String -> D.TextData -> JSONShapeData
shapeEncodeTextData ps s (D.TextData{D.font = f, D.size = si, D.italic = i, D.alignment = a, D.underline = u}) = result
        where
            pointData = shapeEncodePoint ps
            al = case a of 
                        D.Start -> Just "left"
                        D.Center -> Just "center"
                        D.End -> Just "right"

            text = pointData{text= Just $ T.pack $ s}
            result = text{fontFamily= Just $ T.pack $ f, fontSize = Just si, align = al}



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
