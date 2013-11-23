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
        describe "Containers" $ do
            it "can be an empty container" $ do
                shapeDec (Container 1 2 []) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\":[]}"
            it "can place a basic shape in a container" $ do
                shapeDec (Container 1 2 [(Circle (3,4) 5)]) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can place basic shapes in a container" $ do
                shapeDec (Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can place containers in containers" $ do
                shapeDec (Container 10 11 [Circle (12,13) 14, Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]]) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 10, \"height\": 11, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 12, \"y\": 13, \"radius\": 14, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}, {\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}]}"
        describe "Fill" $ do
            it "can change the fill on a basic shape" $ do
                shapeDec (Fill (4,5,6,0.5) (Circle (1,2) 3)) `shouldBe` textDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}}"
            it "can recursively change the fill on a containter and its contents" $ do
                shapeDec (Fill (4,5,6,0.5) (Container 10 11 [Circle (12,13) 14, Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]])) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 10, \"height\": 11, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 12, \"y\": 13, \"radius\": 14, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}}, {\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}}]}]}"
        describe "Stroke" $ do
            it "can add stroke to a basic shape" $ do
                shapeDec (Stroke (4,5,6,0.5) 1 (Rect (1,2) 3 4)) `shouldBe` textDec "{\"type\": \"rect\", \"data\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}}"
            it "can recursively add stroke on a container and its contents" $ do
                shapeDec (Stroke (4,5,6,0.5) 1 (Container 10 11 [Circle (12,13) 14, Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]])) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 10, \"height\": 11, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 12, \"y\": 13, \"radius\": 14, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}}, {\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}}]}]}"
        describe "Rotate" $ do
            it "can rotate a basic shape" $ do
                shapeDec (Rotate 4 (Circle (1,2) 3)) `shouldBe` textDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"rotationDeg\": 4}}"
            it "can rotate a container" $ do
                shapeDec (Rotate 4 (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"rotationDeg\": 4}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can rotate a negative angle" $ do
                shapeDec (Rotate (-4) (Circle (1,2) 3)) `shouldBe` textDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"rotationDeg\": -4}}"    
        describe "Translate" $ do
            it "can translate a basic shape" $ do
                shapeDec (Translate 10 20 (Rect (1,2) 3 4)) `shouldBe` textDec "{\"type\": \"rect\", \"data\": {\"x\": 11, \"y\": 22, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can translate a container" $ do
                shapeDec (Translate 10 20 (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 10, \"y\": 20, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can translate a neagtive amount" $ do
                shapeDec (Translate (-5) (-10) (Rect (20,30) 3 4)) `shouldBe` textDec "{\"type\": \"rect\", \"data\": {\"x\": 15, \"y\": 20, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
        describe "Scale" $ do
            it "can scale basic shapes" $ do
                shapeDec (Scale 2.0 0.5 (Circle (1,2) 3)) `shouldBe` textDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"scaleX\": 2.0, \"scaleY\": 0.5}}"
            it "can scale containers" $ do
                shapeDec (Scale 2.0 0.5 (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"scaleX\": 2.0, \"scaleY\": 0.5}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
        describe "Offset" $ do
            it "can set offset on a basic shape" $ do
                shapeDec (Offset 5 6 (Rect (1,2) 3 4)) `shouldBe` textDec "{\"type\": \"rect\", \"data\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"offset\": [5,6]}}"
            it "can set offset on a container" $ do
                shapeDec (Offset 5 6 (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` textDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"offset\": [5,6]}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            
         -- TODO: Text, Event   
            
            
            
            
---------------------- HELPERS ----------------------

-- convenience functions to decodeJson
textDec :: T.Text -> Maybe Value
textDec  = (decode . B.fromString . T.unpack)

shapeDec :: Shape -> Maybe Value
shapeDec  = (textDec . encode)