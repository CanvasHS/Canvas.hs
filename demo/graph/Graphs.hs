import CanvasHs
import CanvasHs.Data
import Commons
import Debug.Trace
import Prelude

-- Onze state is een getal
data State = State { width :: Int, height :: Int, off :: (Int, Int), selected :: Selected, nodes :: [Node], edges :: [Edge], startNode :: Node }

type Node = (Int, Int)

data Edge = Edge Node Node

data Selected = MoveNode | DeleteNode | AddNode | MoveEdge | DeleteEdge | AddEdge | None
    deriving(Show, Eq)

-- De initiele state
initState = State 900 600 (0,0) None [] [] (-1,-1)

-- registreer jezelf bij Canvas.hs met de functie handler en de initiele state 0
main = installEventHandler handler initState

-- de functie die vanuit Canvas.hs aangeroepen wordt
handler :: State -> Event -> (State, Output)
handler state StartEvent = (state, Out (Just $ drawAll state, [DisplayType FullWindow, Debug False]))
handler state (WindowResize w h) = trace (show w ++ " " ++ show h) $ (newState, shape $ drawAll newState)
    where
        newState = state{width=w, height=h}

handler state (MouseClick (x,y) id)
    | id == "up" = returnAndRender $ updateXY state (0, -10)
    | id == "down" = returnAndRender $ updateXY state (0, 10)
    | id == "left"  = returnAndRender $ updateXY state (10, 0)
    | id == "right" = returnAndRender $ updateXY state (-10, 0)
    | id == "moveNode" = returnAndRender state{selected=MoveNode, startNode=(-1,-1)}
    | id == "addNode" = returnAndRender state{selected=AddNode, startNode=(-1,-1)}
    | id == "deleteNode" = returnAndRender state{selected=DeleteNode, startNode=(-1,-1)}
    | id == "addEdge" = returnAndRender state{selected=AddEdge, startNode=(-1,-1)}
    | id == "deleteEdge" = returnAndRender state{selected=DeleteEdge, startNode=(-1,-1)}
    | id == "area" && ((selected $ state) == AddNode) = returnAndRender $ addNodeToState state{startNode=(-1,-1)} (x,y)
    | startsWith "node" id && (selected $ state) == DeleteNode =  returnAndRender $ removeNodeFromState state{startNode=(-1,-1)} (read $ drop 4 id :: Int) 
    | startsWith "node" id &&
         (startNode state) == (-1, -1) && 
            (selected $ state) == AddEdge =  returnAndRender $ addStartNodeToState state (read $ drop 4 id :: Int)
    | startsWith "node" id &&
         (selected $ state) == AddEdge =  returnAndRender $ addEdge state (read $ drop 4 id :: Int)  
    | startsWith "node" id &&
         (startNode state) == (-1, -1) && 
            (selected $ state) == MoveNode =  returnAndRender $ addStartNodeToState state (read $ drop 4 id :: Int)
    | startsWith "edge" id &&
         (selected $ state) == DeleteEdge = returnAndRender $ removeEdge state (read $ drop 4 id :: Int)
    | id == "area" &&
         (selected $ state) == MoveNode &&
            not ((startNode state) == (-1, -1)) = returnAndRender $ moveNode state (x,y)    
    | otherwise  = returnAndRender $ state{startNode=(-1,-1)}

    where
        updateXY :: State -> (Int, Int) -> State
        updateXY state (x,y) = state{off=(oX+x, oY+y)}
            where
                (oX, oY) = off state

        addNodeToState :: State -> (Int, Int) -> State
        addNodeToState state (x,y) = trace (show (x, y, oX, oY)) newState
            where
                (oX, oY) = off state
                w = width state
                h = height state
                node = (x - oX + ((2000 - w) `div` 2), y - oY + ((2000 - h) `div` 2))
                oldNodes = nodes state
                newState = state{nodes=node:oldNodes}

        removeNodeFromState :: State -> Int -> State
        removeNodeFromState state number = newState
            where
                oldNodes = nodes state
                (start, end) = splitAt number oldNodes
                remNode = end !! 0
                newNodes = start ++ (drop 1 end)
                eds = edges state
                newEdges = filter (\(Edge a b) -> (not (a == remNode || b == remNode))) eds
                newState = state{nodes=newNodes, edges=newEdges}

        addStartNodeToState :: State -> Int -> State
        addStartNodeToState state number = state{startNode=(ns !! number)}
            where
                ns = nodes state
        addEdge :: State -> Int ->  State
        addEdge state number = newState
            where
                prev = startNode state
                ns = nodes state
                es = edges state
                edge = Edge prev (ns !! number)
                newState = state{startNode=(-1,-1), edges=edge:es}

        moveNode :: State -> (Int, Int) -> State
        moveNode state (x,y) = trace (show (x, y, oX, oY)) newState
            where
                (xToBeMoved, yToBeMoved) = startNode state
                (oX, oY) = off state
                w = width state
                h = height state
                node = (x - oX + ((2000 - w) `div` 2), y - oY + ((2000 - h) `div` 2))
                oldNodes = filter (\(x,y) -> not (x == xToBeMoved && y == yToBeMoved)) (nodes state)
                newEdges = fixEdges (edges state) (xToBeMoved, yToBeMoved) node
                newState = state{nodes=node:oldNodes, edges=newEdges, startNode=(-1,-1)}

        fixEdges :: [Edge] -> Node -> Node -> [Edge]
        fixEdges [] _ _ = []
        fixEdges ((Edge (x1, y1) (x2, y2)):xs) (x,y) np
            | x1 == x && y1 == y = (Edge np (x2, y2)):(fixEdges xs (x,y) np)
            | x2 == x && y2 == y = (Edge (x1, y1) np):(fixEdges xs (x,y) np)
            | otherwise = (Edge (x1, y1) (x2, y2)):(fixEdges xs (x,y) np)

        removeEdge :: State -> Int -> State
        removeEdge state id = newState
            where
                (start, end) = splitAt id (edges state)
                newEdges = start ++ (drop 1 end)
                newState = state{edges=newEdges}

handler state _ = (state, shape $ drawAll state)

returnAndRender :: State -> (State, Output)
returnAndRender state = (state, shape $ drawAll state)

drawAll :: State -> Shape
drawAll state = Container w h [
        Fill uiLightGrey $ Rect (0,0) w h,
        drawArea state,
        Translate (w - 200) 0 $ drawToolbox state,
        Translate (w - 175) (h - 175) $ drawMovementControls
    ]
    where
        w = width state
        h = height state

drawArea :: State -> Shape
drawArea state = Translate ((w `div` 2) + x) ((h `div` 2) + y) $ Offset (1000, 1000) $ Container 2000 2000 $ [
        Event defaults{eventId="area", mouseClick=True} $ Fill uiWhite $ Stroke uiGrey 1 $ Rect (0,0) 2000 2000
    ] ++ (map (\x -> drawEdge state x) [0..countEds]) ++ (map (\x -> drawNode state x) [0..countNods])
    where
        (x,y) = off state
        w = width state
        h = height state
        nods = nodes state
        countNods = length nods - 1
        eds = edges state
        countEds = length eds - 1

drawNode :: State -> Int -> Shape
drawNode state number = Event defaults{eventId="node"++(show number), mouseClick=True} $ Fill (if shouldFill then uiBlue else uiWhite) $ Stroke uiBlue 2 $ Circle (x,y) 50
    where
        (x,y) = (nodes state) !! number
        shouldFill = ((startNode state) == (x,y))

drawEdge :: State -> Int -> Shape
drawEdge st num = Translate (minX) (minY) $ Event defaults{eventId="edge"++(show num), mouseClick=True} $ Offset (5,5) $ Container width height $ [ 
        Stroke uiBlue 2 $ Line [(x1 - minX + 5, y1 - minY + 5), (x2 - minX + 5, y2 - minY + 5)] 
    ] ++ (if shouldDisplayGuides then [Fill uiRed $ Circle (width `div` 2, height `div` 2) 8] else [])
    where
        (Edge (x1, y1) (x2, y2)) = (edges st) !! num
        width = abs (x1 - x2) + 10
        height = abs (y1 - y2) + 10
        minX = minimum [x1, x2]
        minY = minimum [y1, y2]
        shouldDisplayGuides = (selected st) == DeleteEdge

drawToolbox :: State -> Shape
drawToolbox state = Container 200 h [
        Fill (255,255,255,0.7) $ Rect (0,0) 200 h,
        Container 200 220 [
            Fill uiGrey $ Text (100,0) "Nodes" defaults{font="Cantarell", size=40, alignment=AlignCenter},
            Stroke uiBlue 1 $ Line [(0,40), (200,40)],
            Translate 0 40 $ Container 200 180 [
                Translate 0 0 $ Event defaults{eventId="moveNode", mouseClick=True} $ Container 200 60 [
                        Stroke uiBlue 1 $ Fill (if sel == MoveNode then uiBlue else uiNone) $ roundedRect (10,10) 180 40 10,
                        Fill (if sel == MoveNode then uiWhite else uiGrey) $ Text (100,20) "Move" defaults{font="Cantarell", size=20, alignment=AlignCenter}
                    ],
                Translate 0 60 $ Event defaults{eventId="addNode", mouseClick=True} $ Container 200 60 [
                        Stroke uiBlue 1 $ Fill (if sel == AddNode then uiBlue else uiNone) $  roundedRect (10,10) 180 40 10,
                        Fill (if sel == AddNode then uiWhite else uiGrey) $ Text (100,20) "Add" defaults{font="Cantarell", size=20, alignment=AlignCenter}
                    ],
                Translate 0 120 $ Event defaults{eventId="deleteNode", mouseClick=True} $ Container 200 60 [
                        Stroke uiBlue 1 $ Fill (if sel == DeleteNode then uiBlue else uiNone) $  roundedRect (10,10) 180 40 10,
                        Fill (if sel == DeleteNode then uiWhite else uiGrey) $ Text (100,20) "Delete" defaults{font="Cantarell", size=20, alignment=AlignCenter}
                    ]
            ]
        ],
        Translate 0 220 $ Container 200 220 [
            Fill uiGrey $ Text (100,0) "Edges" defaults{font="Cantarell", size=40, alignment=AlignCenter},
            Stroke uiBlue 1 $ Line [(0,40), (200,40)],
            Translate 0 40 $ Container 200 120 [
                Translate 0 0 $ Event defaults{eventId="addEdge", mouseClick=True} $ Container 200 60 [
                        Stroke uiBlue 1 $ Fill (if sel == AddEdge then uiBlue else uiNone) $  roundedRect (10,10) 180 40 10,
                        Fill (if sel == AddEdge then uiWhite else uiGrey) $ Text (100,20) "Add" defaults{font="Cantarell", size=20, alignment=AlignCenter}
                    ],
                Translate 0 60 $ Event defaults{eventId="deleteEdge", mouseClick=True} $ Container 200 60 [
                        Stroke uiBlue 1 $ Fill (if sel == DeleteEdge then uiBlue else uiNone) $  roundedRect (10,10) 180 40 10,
                        Fill (if sel == DeleteEdge then uiWhite else uiGrey) $ Text (100,20) "Delete" defaults{font="Cantarell", size=20, alignment=AlignCenter}
                    ]
            ]
        ]
    ]
    where
        sel = selected state
        h = height state
        

        

drawMovementControls :: Shape
drawMovementControls = Container 150 150 [
        Stroke uiBlue 2 $ Event defaults{eventId="up", mouseClick=True} $ Translate 50 0 $ roundedRect (1,1) 49 49 10,
        Stroke uiBlue 2 $ Event defaults{eventId="left", mouseClick=True} $ Translate 0 50 $ roundedRect (1,1) 49 49 10,
        Stroke uiBlue 2 $ Event defaults{eventId="right", mouseClick=True} $ Translate 100 50 $ roundedRect (1,1) 49 49 10,
        Stroke uiBlue 2 $ Event defaults{eventId="down", mouseClick=True} $ Translate 50 100 $ roundedRect (1,1) 49 49 10
    ]

startsWith :: Ord a => [a] -> [a] -> Bool
startsWith _ []            = False
startsWith [] _            = True
startsWith needle haystack = (head needle == head haystack) && (startsWith (tail needle) (tail haystack))

uiBlue = (0,122,255,1.0)
uiGrey = (64,64,64,1.0)
uiRed = (255, 0, 102, 1.0)
uiLightGrey = (224, 224, 224, 1.0)
uiNone = (0,0,0,0.0)
uiWhite = (255,255,255,1.0)
