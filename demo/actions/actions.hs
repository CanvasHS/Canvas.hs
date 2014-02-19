import CanvasHs
import CanvasHs.Data
import Debug.Trace
import Prelude
 
data St
    = St {
        debug :: Bool,
        dragndrop :: Bool,
        timer :: Int,
        colour :: String,
        ballx :: Int,
        bally :: Int,
        ballvx :: Int,
        ballvy :: Int,
        windowWidth :: Int,
        windowHeight :: Int
    } deriving (Show)
 
main = installEventHandler h (St{debug=False, timer=0, dragndrop=False, colour="", ballx=30, bally=30, ballvx=15, ballvy=9, windowWidth=900, windowHeight=600})
 
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
    = (st, Out (Nothing, [DisplayType $ FixedSize 900 600]))
    
h st (WindowResize w h)
    = (st',  shape $ drawCanvas st')
        where st' = st{windowHeight=h, windowWidth=w}
    
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
        st' = st{dragndrop=d'}
-- | a file has been uploaded using either drag'n'drop or RequestUpload, we treat it as a timer value 
       
h st (UploadComplete (con, _))
    = (st', shape $ drawCanvas st')
        where
        t = read con
        st' = st{timer=t}
        
h st (MouseClick _ "bounceball")
    = (st, Out (Nothing, [Timer 120 "ball"]))
h st (Tick "ball")
    = (st', shape $ drawCanvas st')
        where
        st' = calculateBall st

h st (MouseClick _ "prompt")
    = (st, Out (Nothing, [Prompt "What is your favourite colour?" "green"]))
h st (PromptResponse ans)
    = (st', shape $ drawCanvas st')
        where st' = st{colour = ans}

h st _ = h st StartEvent
 
drawCanvas :: St -> Shape
drawCanvas st   = Container 900 600 [drawButtons
                                    ,Translate 175 25 $ drawState st
                                    ,Translate 175 120 $ drawBounce st
                                    ]
 
-- | Draws a container of size w:200 h:60 displaying the current state
drawState :: St -> Shape
drawState st = Fill darkred $ Container 200 100 
                                [Text (0,0)     ("Timer: " ++ show t) defaults{font="Helvetica", alignment=AlignLeft, bold=True}
                                ,Text (0,20)    ("Debug: " ++ show d) defaults{font="Helvetica", alignment=AlignLeft}
                                ,Text (0,40)    ("Drag'n'Drop: " ++ show dnd) defaults{font="Helvetica", alignment=AlignLeft, italic=True}
                                ,Text (0,60)    ("Favourite colour: " ++ clr) defaults{font="Helvetica", alignment=AlignLeft, bold=True}
                                ,Text (0,80)    ("window size (w,h): ("++show w++","++show h++")") defaults{font="Helvetica", alignment=AlignLeft}
                                ]
                where
                St{timer=t, debug=d, dragndrop=dnd, colour=clr, windowHeight=h, windowWidth=w} = st
    
-- | returns a container at position 0 0 of width 125, height 500 containing all buttons
drawButtons :: Shape    
drawButtons = Container 150 500 [Translate 25 0     $ button "Toggle debug" "debug"
                                ,Translate 25 40    $ button "Fullscreen" "fullscreen"
                                ,Translate 25 80    $ button "Fullwindow" "fullwindow"
                                ,Translate 25 120   $ button "Fixedsize 900 600" "fixedsize"
                                ,Translate 25 160   $ button "Start timer" "timer"
                                ,Translate 25 200   $ button "Save to timer.txt" "save"
                                ,Translate 25 240   $ button "Load from timer.txt" "load"
                                ,Translate 25 280   $ button "Download timer" "download"
                                ,Translate 25 320   $ button "Request upload" "upload"
                                ,Translate 25 360   $ button "Toggle drag'n'drop" "dragndrop"
                                ,Translate 25 400   $ button "Start bounce" "bounceball"
                                ,Translate 25 440   $ button "prompt favourite colour" "prompt"
                                ]
 
-- | draws a button at position 0 0, with the first string as label and the second as event id
button :: String -> String -> Shape
button lab id   = Event defaults{eventId=id, mouseClick=True} $
                     Container 125 30 [Stroke darkblue 5 $ Fill blue $ Rect (0,0) 125 30
                                       ,Fill darkblue $ Text (5,5) lab defaults{font="Helvetica", alignment=AlignLeft}
                                       ]
 
-- | draws a container of 400 400 containing a bouncing ball
drawBounce :: St -> Shape
drawBounce st = Container 400 400   [Stroke darkred 10 $ Fill magenta $ Rect (0,0) 400 400
                                    ,Fill darkred $ Circle (x,y) 30
                                    ]
                where
                St{ballx=x, bally=y} = st
                
calculateBall :: St -> St
calculateBall st = st{ballx=x', bally=y', ballvx=vx', ballvy=vy'}
                    where
                    St{ballx=x, bally=y, ballvx=vx, ballvy=vy} = st
                    vx' = if ((x + vx) > 370) || ((x + vx) < 30) then negate vx else vx
                    vy' = if ((y + vy) > 370) || ((y + vy) < 30) then negate vy else vy
                    x' = x + vx
                    y' = y + vy
                                
 
-- Color helper functions
black = (0,0,0,1.0)
gray = (207,207,197,1.0)
blue = (174,198,207,1.0)
darkblue = (100,130,180,1.0)
darkred = (194,59,34,1.0)
magenta = (244,154,194,1.0)
