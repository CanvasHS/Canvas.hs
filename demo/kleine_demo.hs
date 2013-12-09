import CanvasHs
import CanvasHs.Data

main = installEventHandler handl 0

handl :: Int -> Event -> (Int, Shape)
handl num StartEvent = (num, Container 900 600 [
                                Event defaults{eventId="a",mouseOut=True, mouseOver=True} $
                                    Circle (200, 200) 40
                        ])

handl num (MouseOut (x,y) id) = (num, Container 900 600 [
                                 Event defaults{eventId="a",mouseOut=True, mouseOver=True} $
                                    Fill (255, 0, 0, 1.0) $ Circle (200, 200) 40
                                ])

handl num (MouseOver (x,y) id) = (num, Container 900 600 [
                                 Event defaults{eventId="a",mouseOut=True, mouseOver=True} $
                                    Fill (0, 255, 0, 1.0) $ Circle (200, 200) 40
                                ])
