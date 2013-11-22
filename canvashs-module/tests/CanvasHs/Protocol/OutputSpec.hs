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
        describe "Fill" $ do
            it "can change the fill on a basic shape" $ do
                shapeDec (Fill (4,5,6,0.5) (Circle (1,2) 3)) `shouldBe` textDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}}"
            it "can recursively change the fill on a containter" $ do
                pending
        describe "Containers" $ do
            it "can be an empty container" $ do
                shapeDec (Container 1 2 []) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\":[]}"
            it "can place a basic shape in a container" $ do
                shapeDec (Container 1 2 [(Circle (3,4) 5)]) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can place basic shapes in a container" $ do
                shapeDec (Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can place containers in containers" $ do
                shapeDec (Container 10 11 [Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]]) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 10, \"height\": 11, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}]}"
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
---------------------- HELPERS ----------------------

-- convenience functions to decodeJson
textDec :: T.Text -> Maybe Value
textDec  = (decode . B.fromString . T.unpack)

shapeDec :: Shape -> Maybe Value
shapeDec  = (textDec . encode)