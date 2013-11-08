import CanvasHs

import CanvasHs.Data
import Debug.Trace
import KaartData
import Buttons

data State = State {
    xDiff :: Int,
    yDiff :: Int,
    zoom :: Float
}

emptyState = State 0 0 0.4

main = installEventHandler handl emptyState

-- we gebruiken een simpele Int als store (maar het zou ook een record oid kunnen zijn)
handl :: State -> Event -> (State, Shape)
handl st@State{zoom=zoom} StartEvent = (st, 
    Container 900 600 
        [
            drawBackground,
            Scale zoom zoom $ drawMap,
            drawControls zoom
        ])

handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom} (MouseClick (x,y) ev) = (newState,
    Container 900 600
        [
            drawBackground,
            (Translate xNew yNew $ Scale newZoom newZoom $ drawMap),
            drawControls newZoom
        ])
    where
        (x, y) = translateFromEvent ev
        (xNew, yNew) = (x + xDiff, y + yDiff) 
        newZoom = scaleFromEvent zoom ev
        newState = st{xDiff=xNew, yDiff=yNew, zoom=newZoom}
  

drawBackground :: Shape
drawBackground = Fill (135,206,235,1.0) $ Rect (0,0) 900 600

drawMap :: Shape
drawMap =  Container 1200 1536 polys
    

drawControls :: Float -> Shape
drawControls zl =
    Translate (900 - 112) 16 $ Container 96 288 [
        drawMovementControls,
        Translate 32 128 $ drawZoomControls zl
    ]

drawMovementControls :: Shape
drawMovementControls =
    Container 96 96 [
        -- arrow right
        Event defaults{eventId="lt",mouseClick=True} $ Translate  0 32 $ button,
        -- arrow up 
        Event defaults{eventId="up",mouseClick=True} $ Translate 32  0 $ button, 
        -- arrow left
        Event defaults{eventId="dn",mouseClick=True} $ Translate 32 64 $ button, 
        -- arrow down
        Event defaults{eventId="rt",mouseClick=True} $ Translate 64 32 $ button
    ]

drawZoomControls :: Float -> Shape
drawZoomControls zl =
    Container 32 160 [
        Event defaults{eventId="zi",mouseClick=True} $ Translate 0 0 $ button,
        Fill (255,255,255,0.75) $ Rect (0, 40 + zoomY) 32 16,
        Event defaults{eventId="zo",mouseClick=True} $ Translate 0 128 $ button
    ]
    where
        zoomPerc = (zl - 0.4) / 0.6
        zoomY = round $ (1.0 - zoomPerc) * 64.0


tfcom :: Shape -> String -> Shape
tfcom s c = case c of
            "up" -> (Translate 0 (-50) $ s)
            "lt" -> (Translate (-50) 0 $ s)
            "rt" -> (Translate 50 0 $ s)
            "dn" -> (Translate 0 50 $ s)
            _    -> error("dit kan niet")

translateFromEvent :: String -> (Int, Int)
translateFromEvent c = case c of
            "up" -> (0, 50)
            "lt" -> (50,0)
            "rt" -> (-50,0)
            "dn" -> (0,-50)
            _    -> (0,0)

scaleFromEvent :: Float -> String -> Float
scaleFromEvent prev ev = case ev of
            "zi" -> min (prev + 0.05) (1.0)
            "zo" -> max (prev - 0.05) (0.4)
            _    -> prev
    
