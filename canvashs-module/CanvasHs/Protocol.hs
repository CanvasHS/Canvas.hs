{-# LANGUAGE OverloadedStrings #-}

{- |
	Deze module handelt het omzetten van het interne datamodel naar het protocol af d.m.v. de
	encode-functie en het omzetten de andere kant op d.m.v. de decode-functie
	
	Op dit moment is de module in dummy-staat om de inrichting van de code duidelijk te hebben,
	de daadwerkelijke invulling (body) van de functies zal nog veranderen.
-}
module CanvasHs.Protocol
(	encode
	,decode
) where 

import CanvasHs.Data
import qualified Data.Text as T
import Data.List (intercalate)

-- | encode maakt van een Output een JSON-string (type Data.Text.Text) die voldoet aan het protocol
--	 @ensure \result is een valide JSON-object
--   LET OP: het resultaat voldoet momenteel NIET aan het protocol!
encode :: Shape -> T.Text
encode (Circle p r)		= T.pack $ unlines [
							"{"
							,"    \"type\": \"circle\","
							,"    \"data\": {"
							,"        \"id\": \"circle_nr_1\","
							,"        \"x\": " ++ (encodePointX p) ++ ","
							,"        \"y\": " ++ (encodePointY p) ++ ","
							,"        \"radius\": "++(show r)
							,"    }"
							,"}"]
--encode (Rect p w h) 	= T.pack $ unlines
--							["{shape : 'rect'"
--							,",origin : " ++ (encodePoint p)
--							,",width : " ++ (show w)
--							,",height : " ++ (show h)
--							,"}"
--							]
--encode (Line ps)		= T.pack $ unlines
--							["{shape : 'line'"
--							,",path : [" ++ (intercalate "," $ map encodePoint ps) ++ "]"
--							,"}"
--							]
--encode (Fill c s)		= T.pack $ unlines
--							["{effect : 'fill'"
--							,",color : " ++ (encodeColor c)
--							,",shape : " ++ T.unpack (encode s)
--							,"}"
--							]
						

encodePointX :: Point -> String
encodePointX (x,_) = show x
encodePointY :: Point -> String
encodePointY (_,y) = show y

encodeColor :: Color -> String
encodeColor (r,g,b,a) = concat ["{red : ", show r, ", green : ", show g, ", blue : ", show b, ", alpha : ", show a, "}"]

-- | Ontsleuteld een inkomend bericht naar een event
--	 Moet nog daadwerkelijk geïmplementeerd worden. Dat zal uiteindelijk met een eigen parser moeten
decode :: T.Text -> EventData
decode _ = defaults
