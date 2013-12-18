-- Canvas.Hs, control javascript canvas with Haskell
-- Copyright (C) 2013, Lennart Buit, Joost van Doorn, Pim Jager, Martijn Roo,
-- Thijs Scheepers
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public
-- License as published by the Free Software Foundation; either
-- version 2.1 of the License, or (at your option) any later version.
-- 
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- Lesser General Public License for more details.
-- 
-- You should have received a copy of the GNU Lesser General Public
-- License along with this library; if not, write to the Free Software
-- Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
-- USA

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
import CanvasHs.Protocol.ActionOutput
import CanvasHs.Protocol.Input

import qualified Data.Text as T
import Data.List (intercalate)
import qualified Data.Aeson as Aeson (encode, eitherDecode)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.UTF8 as BUL
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))

data JSONOutput = JSONOutput {
        shape :: Maybe JSONShape,
        actions :: [JSONAction]
        } deriving (Show)
        
$(deriveJSON defaultOptions{omitNothingFields=True} ''JSONOutput)

{-  |
    encode maakt van een Output een JSON-string (type Data.Text) die voldoet aan het protocol
    @ensure \result is een valide JSON-object
-}
encode :: RegularOutput -> BU.ByteString
encode o = BS.concat $ BSL.toChunks $ Aeson.encode (JSONOutput {shape=(shapeEncode <$> fst o), actions=(map actionEncode $ snd o)})

-- | Ontsleuteld een inkomend bericht naar een event
--   De daadwerkelijke code hiervoor staat in CanvasHs.Protocol.Output
decode :: BU.ByteString -> Event
decode "INIT"   = StartEvent
decode s        = either (\b -> error $ "Aeson decode error: "++b) (\b -> b) $ Aeson.eitherDecode $ BSL.fromChunks [s]
