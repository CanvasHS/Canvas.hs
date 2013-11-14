import CanvasHs

import Prelude
import CanvasHs.Data
import Debug.Trace
import KaartData
import Buttons

data State = State {
    xDiff :: Int,
    yDiff :: Int,
    zoom :: Float,
    searchHasFocus :: Bool,
    searchText :: String
}

emptyState = State 0 0 0.4 False ""

main = installEventHandler handl emptyState

-- we gebruiken een simpele Int als store (maar het zou ook een record oid kunnen zijn)
handl :: State -> Event -> (State, Shape)

handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom} StartEvent = (st, 
    Container 900 600 
        [
            drawBackground,
            Translate xDiff yDiff $ Scale zoom zoom $ drawMap,
            drawControls zoom
        ])


handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom, searchHasFocus=searchHasFocus} (MouseClick (x,y) ev) = (newState,
    Container 900 600
        [
            drawBackground,
            (Translate xNew yNew $ Scale newZoom newZoom $ drawMap),
            drawControls newZoom
        ])
    where
        (x, y) = translateFromEvent ev
        (xNew, yNew) = (x + xDiff, y + yDiff) 
        newFocus = (ev == "search")
        newZoom = scaleFromEvent zoom ev
        newState = st{xDiff=xNew, yDiff=yNew, zoom=newZoom, searchHasFocus=newFocus}

handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom} (MouseOver (x,y) naam) = (st,
    Container 900 600
        [
            drawBackground,
            Translate xDiff yDiff $ Scale zoom zoom $ drawMap,
            drawControls zoom,
            Text (0, 0) naam defaults
        ])

handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom} (MouseOut (x,y) naam) = (st,
    Container 900 600
        [
            drawBackground,
            Translate xDiff yDiff $ Scale zoom zoom $ drawMap,
            drawControls zoom,
            Text (500, 100) "Ik doe shit en ben daar mega gelukkig over" defaults{font="Cantarell", size=20}
        ])

handl st (KeyDown 'c' a) = (st, Container 900 600 [Text (500, 100) "hoi" defaults])

drawBackground :: Shape
drawBackground = Fill (135,206,235,1.0) $ Rect (0,0) 900 600

drawMap :: Shape
drawMap =  Container 1200 1536 (nederland ++ steden)

drawSearch :: Shape
drawSearch = 
    Event defaults{eventId="search", mouseClick=True} $ Translate 5 5 $ Container 300 45 $ [
        Fill (255, 255, 255, 1.0) $ Rect (0,0) 300 45,
        Translate 260 5 $ Container 35 35 $ [
            Fill (128, 128, 128, 1.0) $ Circle (15, 15) 15,
            Fill (128, 128, 128, 1.0) $ Polygon [(20, 25), (30, 35), (35, 30), (25, 20)],
            Fill (255, 255, 255, 1.0) $ Circle (15, 15) 10
        ]
    ]
        

drawControls :: Float -> Shape
drawControls zl =
    Container 900 600 [
        drawSearch,
        Translate (900 - 112) 16 $ Container 96 288 [
            drawMovementControls,
            Translate 32 128 $ drawZoomControls zl
        ]
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
    
