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
import CanvasHs.Protocol.Output
import CanvasHs.Protocol.Input

import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.Aeson as Aeson (encode, eitherDecode)
import qualified Data.ByteString.Lazy.UTF8 as BU (fromString, toString)
import Data.Either (either)

{- 	|
	encode maakt van een Output een JSON-string (type Data.Text) die voldoet aan het protocol
	@ensure \result is een valide JSON-object
-}
encode :: Shape -> T.Text
encode = T.pack . BU.toString . Aeson.encode . iEncode

-- | Ontsleuteld een inkomend bericht naar een event
--   De daadwerkelijke code hiervoor staat in CanvasHs.Protocol.Output
decode :: T.Text -> Event
decode "INIT"   = StartEvent
decode s        = either (\b -> error $ "Aeson decode error: "++b) (\b -> b) $ Aeson.eitherDecode $ BU.fromString $ T.unpack s
