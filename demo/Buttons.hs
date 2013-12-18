module Buttons where

import Prelude
import CanvasHs.Data

-- button = Container 32 32 [ Fill (255, 255, 255,1.0) $ Polygon [(0, 0), (32,0), (32,28), (28, 32), (5, 32), (0, 28)], Polygon [(4,16), (16,4), (28,16), (24,20), (16,12), (8,20)]]

button = Fill (255,255,255,0.75) $ Rect (1,1) 31 31
