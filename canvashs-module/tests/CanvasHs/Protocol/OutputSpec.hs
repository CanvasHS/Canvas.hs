{-# LANGUAGE OverloadedStrings #-}

module CanvasHs.Protocol.OutputSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck

import CanvasHs.Protocol (encode)
import CanvasHs.Data

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Aeson (decode, Value)


main :: IO ()
main = hspec spec

----------------------- TESTS ----------------------

spec :: Spec
spec = do
    describe "Aeson Value" $ do
        it "is equal on equal JSON" $ do
            textDec "{\"name\":\"JSON\", \"Pizza\":4}" `shouldBe` textDec "{\"name\":\"JSON\", \"Pizza\":4}"
        it "is equal on JSON with different ordering" $ do
            textDec "{\"name\":\"JSON\", \"Pizza\":4}" `shouldBe` textDec "{\"Pizza\":4, \"name\":\"JSON\"}"
        it "is not equal on different fields" $ do
            textDec "{\"name\":\"JSON\"}" /= textDec "{\"name\":\"JSON\", \"Pizza\":4}"
        it "is not equal on different field contents" $ do
            textDec "{\"name\":\"JSON\"}" /= textDec "{\"name\":\"Frans\"}"

    describe "Protocol.encode" $ do
        describe "encode basic shapes" $ do
            it "can encode proper Rectangles" $ do
                shapeDec (Rect (1,2) 3 4) `shouldBe` textDec "{\"type\": \"rect\", \"data\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can encode proper Circles" $ do
                shapeDec (Circle (1,2) 3) `shouldBe` textDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can encode proper Arcs" $ do
                pendingWith "Arcs not yet implemented"
            it "can encode proper Lines" $ do
                shapeDec (Line [(1,2),(3,4),(5,6),(7,8)]) `shouldBe` textDec "{\"type\": \"line\", \"data\": {\"points\": [1,2,3,4,5,6,7,8], \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can encode proper Polygons" $ do
                shapeDec (Polygon [(1,2),(3,4),(5,6),(7,8)]) `shouldBe` textDec "{\"type\": \"polygon\", \"data\": {\"points\": [1,2,3,4,5,6,7,8], \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"   
{-        describe "encode Text" $ do
            it "can encode simple text objects" $ do
                -- -}
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
---------------------- HELPERS ----------------------

-- convenience functions to decodeJson
textDec :: T.Text -> Maybe Value
textDec  = (decode . B.fromString . T.unpack)

shapeDec :: Shape -> Maybe Value
shapeDec  = (textDec . encode)