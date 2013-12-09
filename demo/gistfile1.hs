import CanvasHs

import CanvasHs.Data
import Debug.Trace

main = installEventHandler handl 1


-- we gebruiken een simpele Int als store (maar het zou ook een record oid kunnen zijn)
handl :: Int -> Event -> (Int, Shape)
handl st StartEvent  = (st, Container 900 600 [(Event defaults{eventId="123",mouseClick=True} (Circle (34, 34) 34))
	])    -- Eerste image tekenen
handl st (MouseClick (x,y) "123") = (st, Container 900 600 [(Event defaults{eventId="123",mouseClick=True} (Circle (x, y) 34))])    -- Eerste image tekenen
handl st _      = (st+1, (Circle (1, 1) st))  -- bij elk volgende event tekenen we er een klein cirkeltje bij
