module Chart where

import CanvasHs
import CanvasHs.Data
import Commons
import Debug.Trace

data ChartData = ChartData String [(String, Int)]
    deriving(Show, Eq, Read)

-- Onze state is een getal
data State = State { width :: Int, height :: Int, open :: Int, chart :: ChartData, debug :: Bool, fullscreen :: Bool }

-- De initiele state
initState = State 900 600 0 exampleChart True False

colors = [pastelRed, pastelYellow, pastelBlue, pastelGreen]

-- registreer jezelf bij Canvas.hs met de functie handler en de initiele state 0
main = installEventHandler handler initState    

-- de functie die vanuit Canvas.hs aangeroepen wordt
handler :: State -> Event -> (State, Output)
handler state (MouseClick (x,y) "menu") 
    | o >= 170 = (state, Out ((Just $ drawAll state), [Timer 50 "closeMenu"]))
    | otherwise = (state, Out ((Just $ drawAll state), [Timer 50 "openMenu"]))

    where
        w = width state
        h = height state
        o = open state

handler state (Tick "openMenu")
    | o >= 170 = trace ("maximaal open") $ (state{open=170}, Out (Just (drawAll newState), [StopTimer "openMenu"]))
    | otherwise = trace (show o) $ (newState, shape (drawAll newState))
    where
        w = width state
        h = height state
        o = open state
        newState = state{open=(o+10)}

handler state (Tick "closeMenu")
    | o <= 0 = trace ("maximaal dicht") (state{open=0}, Out (Just (drawAll newState), [StopTimer "closeMenu"]))
    | otherwise = trace (show o) $ (newState, shape (drawAll newState))
    where
        w = width state
        h = height state
        o = open state
        newState = state{open=(o-10)}

handler state (MouseClick (x,y) "fullscreen") = (newState, Out (Just (drawAll newState), [DisplayType wt]))
    where
        oldFs = fullscreen $ state
        fs = not oldFs
        newState = state{fullscreen=fs}
        wt = if fs then FullWindow else FixedSize 900 600

handler state (MouseClick (x,y) "debug") = (newState, Out (Just (drawAll newState), [Debug (newDebug)]))
    where
        d = debug $ state
        newDebug = not d
        newState = state{debug=newDebug}

handler state (MouseClick (x,y) "upload") = (state, Out (Nothing, [RequestUpload False]))

handler state (MouseClick (x,y) "download") = (state, Out (Nothing, [Download "graph.txt" (show $ chart $ state)]))

handler state (UploadComplete (st, bs)) = (newState, shape $ drawAll newState)
    where
        newChart = read st
        newState = state{chart=newChart}

handler state (WindowResize w h) = trace (show w ++ " " ++ show h) $ (newState, shape (drawAll newState))
    where
        newState = state{width=w, height=h}
                

handler state _ = (state, shape $ Container w h [
        drawChartComplete state,
        drawMenu state
    ])

    where
        w = width state
        h = height state

drawAll :: State -> Shape
drawAll state = Container w h [
                drawChartComplete state,
                drawMenu state
            ]
    where
        h = height state
        w = width state

drawMenu :: State -> Shape
drawMenu state = Translate xCont yCont $ Container 600 200 [
        Stroke uiBlue 1 $ Fill (255, 255, 255, 0.75) $  Polygon [(0, 200), (0, 25), (240, 25), (260, 5), (340, 5), (360, 25), (600, 25), (600, 200)],
        Event defaults{eventId="menu", mouseClick=True} $ Translate 270 0 $ Container 60 30 [
            Stroke uiBlue 1 $ Line [(0, 10), (60, 10)],
            Stroke uiBlue 1 $ Line [(0, 15), (60, 15)],
            Stroke uiBlue 1 $ Line [(0, 20), (60, 20)],
            Fill (0, 0, 0, 0.0) $ Rect (0,0) 60 30
        ],
        Translate 10 35 $ Event defaults{eventId="fullscreen", mouseClick=True} $ Container 130 130 [
                Stroke uiBlue 1 $ Fill (if f then uiGreen else uiRed) $ roundedRect (0,0) 130 130 25,
                Fill uiGrey $ Text (65,55) "Fullscreen" defaults{font="Cantarell", size=20, alignment=AlignCenter}
            ],
        Translate 160 35 $ Event defaults{eventId="debug", mouseClick=True} $ Container 130 130 [
                Stroke uiBlue 1 $ Fill (if d then uiGreen else uiRed) $ roundedRect (0,0) 130 130 25,
                Fill uiGrey $ Text (65,55) "Debug" defaults{font="Cantarell", size=20, alignment=AlignCenter}
            ],
        Translate 310 35 $ Event defaults{eventId="upload", mouseClick=True} $ Container 130 130 [
                Stroke uiBlue 1 $ roundedRect (0,0) 130 130 25,
                Fill uiGrey $ Text (65,55) "Upload" defaults{font="Cantarell", size=20, alignment=AlignCenter}
            ],
        Translate 460 35 $ Event defaults{eventId="download", mouseClick=True} $ Container 130 130 [
                Stroke uiBlue 1 $ roundedRect (0,0) 130 130 25,
                Fill uiGrey $ Text (65,55) "Download" defaults{font="Cantarell", size=20, alignment=AlignCenter}
            ]
    ]
    where
        w = width state
        h = height state
        o = open state
        wCont = 600
        hCont = 200
        xCont = (w `div` 2) - (wCont `div` 2)
        yCont = h - o - 25
        d = debug state
        f = fullscreen state

drawChartComplete :: State -> Shape
drawChartComplete state = Container w h [
        Fill uiGrey $ Text (0,0) name defaults{font="Cantarell", size=50},
        Stroke uiBlue 1 $ Line [(0,50), (w,50)],
        drawChart state,
        Translate (w - 200) 50 $ drawLegenda cData
    ]

    where
        w = width state
        h = height state
        cData = chart $ state
        (ChartData name xs) = cData

drawChart :: State -> Shape
drawChart state = Translate borderX borderY $ Container dia dia $ drawChartRec ch radius 0 0
    where
        ch = chart $ state
        w = width state
        h = height state
        marginX = 20
        marginY = 20
        hContainer = h - 50 - 2 * marginY
        wContainer = w - 200 - 2 * marginX
        dia = minimum [hContainer, wContainer]
        radius = dia `div` 2
        borderX = (marginX) + (wContainer - dia) `div` 2
        borderY = (50 + marginY) + (hContainer - dia) `div` 2

drawChartRec :: ChartData -> Int -> Int -> Int -> [Shape]
drawChartRec (ChartData name []) rad rot i = []
drawChartRec (ChartData name (x:xs)) rad rot i = (drawChartRec (ChartData name xs) rad rotNext (i+1)) ++ [Fill (getColor i) $ Rotate rot $ Arc (rad, rad) rad angle]
    where
        angle = snd x
        rotNext = angle + rot
        getColor a = (colors !! (a `mod` (length colors)))

drawLegenda :: ChartData -> Shape
drawLegenda (ChartData name dataPoints) = Container 150 200 $ 
    map (\x -> 
        Translate 0 (x * 30) $ 
            createLegendaItem (getColor x) (fst (dataPoints !! x))
        ) 
    [0..len]

    where
        len = (length dataPoints) - 1
        getColor a = (colors !! (a `mod` (length colors)))

createLegendaItem :: Color -> String -> Shape
createLegendaItem c s = Container 150 30 [
                            Fill c $ Rect (10, 10) 10 10,
                            Fill uiGrey $ Text (30, 10) s defaults{font="Cantarell", size=20}
                        ]

exampleChart = ChartData "Pie Chart" [
        ("Red", 120),
        ("Yellow", 120),
        ("Blue", 120)
    ]

pastelRed = (255, 64, 64, 1.0)
pastelGreen = (64, 255, 64, 1.0)
pastelBlue = (64, 64, 255, 1.0)
pastelYellow = (255, 255, 64 ,1.0)

uiBlue = (0,122,255,1.0)
uiRed = (255,0,0,0.3)
uiGreen = (0,255,0,0.3)
transparentWhite = (255,255,255,0.75)
uiGrey = (64,64,64,1.0)
