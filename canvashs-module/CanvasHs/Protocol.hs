{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
    Deze module handelt het omzetten van het interne datamodel naar het protocol af d.m.v. de
    encode-functie en het omzetten de andere kant op d.m.v. de decode-functie
    
    Op dit moment is de module in dummy-staat om de inrichting van de code duidelijk te hebben,
    de daadwerkelijke invulling (body) van de functies zal nog veranderen.
-}
module CanvasHs.Protocol
(   encode
    ,decode
) where 

import CanvasHs.Data
import CanvasHs.Protocol.ShapeOutput
import CanvasHs.Protocol.Input

import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.Aeson as Aeson (encode, eitherDecode)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.UTF8 as BU (fromString, toString)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

data JSONOutput = JSONOutput {
        shape :: Maybe JSONShape,
        actions :: [String]
        } deriving (Show)
        
$(deriveJSON defaultOptions{omitNothingFields=True} ''JSONOutput)

{-  |
    encode maakt van een Output een JSON-string (type Data.Text) die voldoet aan het protocol
    @ensure \result is een valide JSON-object
-}
encode :: RemoteOutput -> T.Text
encode o = T.pack $ BU.toString $ Aeson.encode (JSONOutput {shape=(shapeEncode <$> fst o), actions=[]})

-- | Ontsleuteld een inkomend bericht naar een event
--   De daadwerkelijke code hiervoor staat in CanvasHs.Protocol.Output
decode :: T.Text -> Event
decode "INIT"   = StartEvent
decode s        = either (\b -> error $ "Aeson decode error: "++b) (\b -> b) $ Aeson.eitherDecode $ BU.fromString $ T.unpack s
