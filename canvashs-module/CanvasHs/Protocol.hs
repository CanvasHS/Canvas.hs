{-# LANGUAGE OverloadedStrings #-}

module CanvasHs.Protocol
(	encode
) where 

import qualified Data.Text as T
import CanvasHs

encode :: Output -> T.Text
encode (Circle p r)		= T.pack $ unlines 
							["{Shape : 'Cicle'" 
							,",origin : " ++ (encodePoint p)
							,",radius : " ++ (show r)
							,"}"
							]
encode (Rect p w h) 	= T.pack $ unlines
							["{shape : 'rect'"
							,",origin : " ++ (encodePoint p)
							,",width : " ++ (show w)
							,",height : " ++ (show h)
							,"}"
							]
encode (Line p1 p2)		= T.pack $ unlines
							["{shape : 'line'"
							,",origin : " ++ (encodePoint p1)
							,",end : " ++ (encodePoint p2)
							,"}"
							]
						

encodePoint :: Point -> String
encodePoint (Point (x,y)) = concat ["{x : ", show x, ", y : ", show y, "}"]
