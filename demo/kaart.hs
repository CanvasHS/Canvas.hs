import CanvasHs

import Prelude
import CanvasHs.Data
import KaartData
import Buttons

import Data.List
import Data.Char

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

handl st StartEvent = (st, drawAll st)

handl st (MouseClick (x,y) "search") = (newState, drawAll newState)
    where
        newState = st{searchHasFocus=True}

handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom} (MouseClick (x,y) ev) = (newState, drawAll newState)
    where
        (x, y) = translateFromEvent ev
        (xNew, yNew) = (x + xDiff, y + yDiff)
        newZoom = scaleFromEvent zoom ev
        newState = st{xDiff=xNew, yDiff=yNew, zoom=newZoom, searchHasFocus=False}


handl st@State{xDiff=xDiff, yDiff=yDiff} (MouseDrag (x1, y1) "rootcontainer" (x2, y2) _) = (newState, drawAll newState)
    where
        (xNew, yNew) = (xDiff + x2 - x1, yDiff + y2 - y1)
        newState = st{xDiff=xNew, yDiff=yNew}

{-
handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom,searchText = s} (MouseOver (x,y) naam) = (st,
    Container 900 600
        [
            drawBackground,
            Translate xDiff yDiff $ Scale zoom zoom $ drawMap,
            drawControls zoom s,
            Text (0, 0) naam defaults
        ])

handl st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom,searchText = s} (MouseOut (x,y) naam) = (st,
    Container 900 600
        [
            drawBackground,
            Translate xDiff yDiff $ Scale zoom zoom $ drawMap,
            drawControls zoom s,
            Text (500, 100) "Ik doe shit en ben daar mega gelukkig over" defaults{font="Cantarell", size=20}
        ])
-}
handl st@State{searchHasFocus = focus, searchText = s} (KeyDown a b) = (newState, drawAll newState)
    where
        newS = case a of
                   "backspace" -> if focus && (length s > 0) then (init s) else s
                   "shift"     -> s
                   "control"   -> s
                   "alt"       -> s
                   "tab"       -> s
                   "space"     -> s ++ " "
                   "insert"    -> s
                   "delete"    -> s
                   "enter"     -> s
                   _           -> if focus then (s ++ a) else s
        newState = st{searchText=newS}

handl st _ = (st, drawAll st)

drawAll :: State -> Shape
drawAll st@State{xDiff=xDiff, yDiff=yDiff, zoom=zoom, searchText=searchText, searchHasFocus=searchHasFocus} =
    Container 900 600
        [
            drawBackground,
            drawMap (xDiff, yDiff) zoom searchText,
            drawControls zoom searchHasFocus searchText
        ]

drawBackground :: Shape
drawBackground = Fill (135,206,235,1.0) $ Rect (0,0) 900 600

drawMap :: (Int, Int) -> Float -> String -> Shape
drawMap (xDiff, yDiff) zoom searchText = Translate (450 + xDiff) (300 + yDiff) $ Scale zoom zoom $ Offset 600 768 $ Event defaults{eventId="rootcontainer", mouseDrag=True} $ Container 1200 1536 (nederland ++ steden ++ (if (length searchText > 0) then (drawCityPopup searchText) else []))

drawControls :: Float -> Bool -> String -> Shape
drawControls zl hasFocus s =
    Container 900 600 [
        drawSearch hasFocus s,
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

drawSearch :: Bool -> String -> Shape
drawSearch hasFocus s = 
    Event defaults{eventId="search", mouseClick=True} $ Translate 5 5 $ Container 300 45 $ [
        searchFill $ Rect (0,0) 300 45,
        Text (5, 5) s defaults{font="Cantarell", size=35},
        Translate 260 5 $ Container 35 35 $ [
            Stroke (128, 128, 128, 1.0) 5 $ Fill (255, 255, 255, 0.1) $ Circle (15, 15) 10,
            Fill (128, 128, 128, 1.0) $ Polygon [(20, 25), (30, 35), (35, 30), (25, 20)]
            --Fill (255, 255, 255, 1.0) $ Circle (15, 15) 10
        ]
    ]
    where
        searchFill = if hasFocus then (Fill (255, 255, 255, 1.0)) else (Fill (255, 255, 255, 0.75))

drawCityPopup :: String -> [Shape]
drawCityPopup name = (map (\(n, (x, y)) ->
            Translate (x - 50)  (y-30) $ Container 100 30 [
                Stroke (0, 0, 0, 1.0) 1 $ Fill (255, 255, 255, 1.0) $ Polygon [(0,0), (100,0), (100, 20), (60,20), (50,30), (40, 20), (0,20)],
                Text (50, 2) n defaults{font="Cantarell", alignment=Center, size=15}
            ]) cities)
    where
        popup = Stroke (0, 0, 0, 1.0) 1 $ Fill (255, 255, 255, 1.0) $ Polygon [(0,0), (100,0), (100, 20), (60,20), (50,30), (40, 20), (0,20)]
        cities = findCities name

findCities :: String -> [(String, (Int, Int))]
findCities name = filter (\(curr, (x,y)) -> (isInfixOf name (map toLower curr))) all_city

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
    
