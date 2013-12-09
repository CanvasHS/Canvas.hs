import CanvasHs
import CanvasHs.Data
import Prelude

data St
    = St {
        debug :: Bool,
        dragndrop :: Bool,
        timer :: Int
    }

main = installEventHandler h (St{debug=False, timer=0, dragndrop=False})

h :: St -> Event -> (St, Output)
h st StartEvent
    = (st, shape $ drawCanvas st)

h st (MouseClick _ "debug")
    = (st', Out (Just $ drawCanvas st', [Debug d']))
        where
            St{debug=d} = st
            d' = not d
            st' = st{debug=d'}
            
h st (MouseClick _ "fullscreen")
    = (st, Out (Nothing, [DisplayType FullScreen]))
    
h st (MouseClick _ "fullwindow")
    = (st, Out (Nothing, [DisplayType FullWindow]))
    
h st (MouseClick _ "fixedsize")
    = (st, Out (Nothing, [DisplayType $ FixedSize 400 300]))
    
h st (MouseClick _ "timer")
    = (st, Out (Nothing, [Timer 1000 "clock"]))
h st (Tick "clock")
    = (st', shape $ drawCanvas st)
        where
        St{timer=t} = st
        st' = st{timer=t+1}
        
h st (MouseClick _ "save")
    = (st, Out (Nothing, [SaveFileString "timer.txt" $ show t]))
        where
        St{timer=t} = st
        
h st (MouseClick _ "load")
    = (st, Block $ LoadFileString "timer.txt")
    
h st (FileLoadedString "timer.txt" c)
    = (st', shape $ drawCanvas st')
        where
        st' = st{timer=read c}
        
h st (MouseClick _ "download")
    = (st, Out (Nothing, [Download "downloaded_timer.txt" $ show t]))
        where
        St{timer=t} = st
        
h st (MouseClick _ "upload")
    = (st, Out (Nothing, [RequestUpload False]))
    
h st (MouseClick _ "dragndrop")
    = (st', Out (Just $ drawCanvas st', [DragNDrop d' False]))
        where
        St{dragndrop=d} = st
        d' = not d
        st' = st{dragndrop=d}
-- | a file has been uploaded using either drag'n'drop or RequestUpload, we treat it as a timer value        
h st (UploadComplete _ (con, _))
    = (st', shape $ drawCanvas st')
        where
        t = read con
        st' = st{timer=t}
        
h st _ = h st StartEvent

drawCanvas :: St -> Shape
drawCanvas st  = Container 900 600 [drawButtons
                                    ,Translate 350 25 $ drawState st
                                    ]

-- | Draws a container of size w:200 h:150 displaying the current state
drawState :: St -> Shape
drawState st = Fill darkred $ Container 200 150 
                                [Text (0,0)    ("Timer: " ++ show t) defaults{font="Helvetica", alignment=Start, bold=True}
                                ,Text (0,50)    ("Debug: " ++ show d) defaults{font="Helvetica", alignment=Start, bold=True}
                                ,Text (0,100)  ("Drag'n'Drop: " ++ show dnd) defaults{font="Helvetica", alignment=Start, bold=True}
                                ]
                where
                St{timer=t, debug=d, dragndrop=dnd} = st
    
-- | returns a container at position 0 0 of width 125, height 500 containing all buttons
drawButtons :: Shape    
drawButtons = Container 125 500 [Translate 25 0    $ button "Toggle debug" "debug"
                                ,Translate 25 40    $ button "Fullscreen" "fullscreen"
                                ,Translate 25 80    $ button "Fullwindow" "fullwindow"
                                ,Translate 25 120  $ button "Fixedsize 400 300" "fixedsize"
                                ,Translate 25 160  $ button "Start timer" "timer"
                                ,Translate 25 200  $ button "Save to timer.txt" "save"
                                ,Translate 25 240  $ button "Load from timer.txt" "load"
                                ,Translate 25 280  $ button "Download timer" "download"
                                ,Translate 25 320  $ button "Request upload" "upload"
                                ,Translate 25 360  $ button "Toggle drag'n'drop" "dragndrop"
                                ]

-- | draws a button at position 0 0, with the first string as label and the second as event id
button :: String -> String -> Shape
button lab id  = Event defaults{eventId=id, mouseClick=True} $
                    Container 100 30 [Stroke darkblue 5 $ Fill blue $ Rect (0,0) 100 30
                                      ,Fill darkblue $ Text (5,5) lab defaults{font="Helvetica", alignment=Start}
                                      ]



-- Color helper functions
black = (0,0,0,1.0)
gray = (207,207,197,1.0)
blue = (174,198,207,1.0)
darkblue = (100,130,180,1.0)
darkred = (194,59,34,1.0)