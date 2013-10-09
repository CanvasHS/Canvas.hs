{-# LANGUAGE OverloadedStrings #-}

{- |
	Deze module handelt het omzetten van het interne datamodel naar het protocol af d.m.v. de
	encode-functie en het omzetten de andere kant op d.m.v. de decode-functie
	
	Op dit moment is de module in dummy-staat om de inrichting van de code duidelijk te hebben,
	de daadwerkelijke invulling (body) van de functies zal nog veranderen.
-}
module CanvasHs.Protocol
(	encode
) where 

import qualified Data.Text as T
import CanvasHs
-- | encode maakt van een Output een JSON-string (type Data.Text.Text) die voldoet aan het protocol
--   LET OP: het resultaat voldoet momenteel NIET aan het protocol!
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
