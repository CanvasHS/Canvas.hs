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

import CanvasHs
import CanvasHs.Data
import qualified Data.Text as T
import Data.List (intercalate)

-- | encode maakt van een Output een JSON-string (type Data.Text.Text) die voldoet aan het protocol
--   LET OP: het resultaat voldoet momenteel NIET aan het protocol!
encode :: Shape -> T.Text
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
encode (Line ps)		= T.pack $ unlines
							["{shape : 'line'"
							,",path : [" ++ (intercalate "," $ map encodePoint ps) ++ "]"
							,"}"
							]
						

encodePoint :: Point -> String
encodePoint (x,y) = concat ["{x : ", show x, ", y : ", show y, "}"]
