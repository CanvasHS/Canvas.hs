{-# LANGUAGE OverloadedStrings #-}

module CanvasHs.Protocol.OutputSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck

import CanvasHs (shape)
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
            shapeTextDec "{\"name\":\"JSON\", \"Pizza\":4}" `shouldBe` shapeTextDec "{\"name\":\"JSON\", \"Pizza\":4}"
        it "is equal on JSON with different ordering" $ do
            shapeTextDec "{\"name\":\"JSON\", \"Pizza\":4}" `shouldBe` shapeTextDec "{\"Pizza\":4, \"name\":\"JSON\"}"
        it "is not equal on different fields" $ do
            shapeTextDec "{\"name\":\"JSON\"}" /= shapeTextDec "{\"name\":\"JSON\", \"Pizza\":4}"
        it "is not equal on different field contents" $ do
            shapeTextDec "{\"name\":\"JSON\"}" /= shapeTextDec "{\"name\":\"Frans\"}"

    describe "Protocol.encode" $ do
        describe "encodes both shapes and actions" $ do
            it "omits Nothing shapes" $ do
                dec (Out (Nothing, [])) `shouldBe` textDec "{\"actions\" : []}"
            it "can encode Actions without Shape" $ do
                actionDec [Debug True] `shouldBe` actionsTextDec "[{\"action\": \"debugger\", \"data\": {\"enabled\": true}}]"
            it "can encode Shape without Actions" $ do
                shapeDec (Rect (1,2) 3 4) `shouldBe` shapeTextDec "{\"type\": \"rect\", \"data\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
    describe "Protocol.encode for shapes" $ do
        describe "encode basic shapes" $ do
            it "can encode proper Rectangles" $ do
                shapeDec (Rect (1,2) 3 4) `shouldBe` shapeTextDec "{\"type\": \"rect\", \"data\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can encode proper Circles" $ do
                shapeDec (Circle (1,2) 3) `shouldBe` shapeTextDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can encode proper Arcs" $ do
                pendingWith "Arcs not yet implemented"
            it "can encode proper Lines" $ do
                shapeDec (Line [(1,2),(3,4),(5,6),(7,8)]) `shouldBe` shapeTextDec "{\"type\": \"line\", \"data\": {\"points\": [1,2,3,4,5,6,7,8], \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can encode proper Polygons" $ do
                shapeDec (Polygon [(1,2),(3,4),(5,6),(7,8)]) `shouldBe` shapeTextDec "{\"type\": \"polygon\", \"data\": {\"points\": [1,2,3,4,5,6,7,8], \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"   
        describe "Containers" $ do
            it "can be an empty container" $ do
                shapeDec (Container 1 2 []) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\":[]}"
            it "can place a basic shape in a container" $ do
                shapeDec (Container 1 2 [(Circle (3,4) 5)]) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can place basic shapes in a container" $ do
                shapeDec (Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can place containers in containers" $ do
                shapeDec (Container 10 11 [Circle (12,13) 14, Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]]) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 10, \"height\": 11, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 12, \"y\": 13, \"radius\": 14, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}, {\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}]}"
        describe "Fill" $ do
            it "can change the fill on a basic shape" $ do
                shapeDec (Fill (4,5,6,0.5) (Circle (1,2) 3)) `shouldBe` shapeTextDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}}"
            it "can recursively change the fill on a containter and its contents" $ do
                shapeDec (Fill (4,5,6,0.5) (Container 10 11 [Circle (12,13) 14, Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]])) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 10, \"height\": 11, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 12, \"y\": 13, \"radius\": 14, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}}, {\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}}}]}]}"
        describe "Stroke" $ do
            it "can add stroke to a basic shape" $ do
                shapeDec (Stroke (4,5,6,0.5) 1 (Rect (1,2) 3 4)) `shouldBe` shapeTextDec "{\"type\": \"rect\", \"data\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}}"
            it "can recursively add stroke on a container and its contents" $ do
                shapeDec (Stroke (4,5,6,0.5) 1 (Container 10 11 [Circle (12,13) 14, Container 1 2 [(Circle (3,4) 5), (Rect (6,7) 8 9)]])) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 10, \"height\": 11, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 12, \"y\": 13, \"radius\": 14, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}}, {\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}},{\"type\": \"rect\", \"data\": {\"x\": 6, \"y\": 7, \"width\": 8, \"height\": 9, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"stroke\": {\"r\":4,\"g\":5,\"b\":6,\"a\":0.5}, \"strokeWidth\": 1}}]}]}"
        describe "Rotate" $ do
            it "can rotate a basic shape" $ do
                shapeDec (Rotate 4 (Circle (1,2) 3)) `shouldBe` shapeTextDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"rotationDeg\": 4}}"
            it "can rotate a container" $ do
                shapeDec (Rotate 4 (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"rotationDeg\": 4}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can rotate a negative angle" $ do
                shapeDec (Rotate (-4) (Circle (1,2) 3)) `shouldBe` shapeTextDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"rotationDeg\": -4}}"    
        describe "Translate" $ do
            it "can translate a basic shape" $ do
                shapeDec (Translate 10 20 (Rect (1,2) 3 4)) `shouldBe` shapeTextDec "{\"type\": \"rect\", \"data\": {\"x\": 11, \"y\": 22, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
            it "can translate a container" $ do
                shapeDec (Translate 10 20 (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 10, \"y\": 20, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
            it "can translate a neagtive amount" $ do
                shapeDec (Translate (-5) (-10) (Rect (20,30) 3 4)) `shouldBe` shapeTextDec "{\"type\": \"rect\", \"data\": {\"x\": 15, \"y\": 20, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}"
        describe "Scale" $ do
            it "can scale basic shapes" $ do
                shapeDec (Scale 2.0 0.5 (Circle (1,2) 3)) `shouldBe` shapeTextDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"scaleX\": 2.0, \"scaleY\": 0.5}}"
            it "can scale containers" $ do
                shapeDec (Scale 2.0 0.5 (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"scaleX\": 2.0, \"scaleY\": 0.5}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
        describe "Offset" $ do
            it "can set offset on a basic shape" $ do
                shapeDec (Offset (5,6) (Rect (1,2) 3 4)) `shouldBe` shapeTextDec "{\"type\": \"rect\", \"data\": {\"x\": 1, \"y\": 2, \"width\": 3, \"height\": 4, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"offset\": [5,6]}}"
            it "can set offset on a container" $ do
                shapeDec (Offset (5,6) (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"offset\": [5,6]}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
        describe "Text" $ do
            it "can encode simple text" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"bold\": false, \"italic\": false, \"underline\": false}}"
            it "can encode with different font family" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{font="comic sans ms"}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"comic sans ms\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"bold\": false, \"italic\": false, \"underline\": false}}"
            it "can encode with different font size" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{size=10}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 10, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"bold\": false, \"italic\": false, \"underline\": false}}"
            it "can encode with start alignment" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{alignment=AlignLeft}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"bold\": false, \"italic\": false, \"underline\": false}}"
            it "can encode with center alignment" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{alignment=AlignCenter}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"center\", \"bold\": false, \"italic\": false, \"underline\": false}}"
            it "can encode with end alignment" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{alignment=AlignRight}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"right\", \"bold\": false, \"italic\": false, \"underline\": false}}"
            it "can encode underline True" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{underline=True}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"underline\": true, \"bold\": false, \"italic\": false}}"
            it "can encode underline False" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{underline=False}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"underline\": false, \"bold\": false, \"italic\": false}}"
            it "can encode italics True" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{italic=True}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"italic\": true, \"bold\": false, \"underline\": false}}"
            it "can encode italics False" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{italic=False}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"italic\": false, \"bold\": false, \"underline\": false}}"
            it "can encode bold True" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{bold=True}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"bold\": true, \"underline\": false, \"italic\": false}}"
            it "can encode bold False" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{bold=False}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"bold\": false, \"underline\": false, \"italic\": false}}"
            it "can encode bold, underline and italic combined" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{bold=True,underline=True,italic=False}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"Arial\", \"text\": \"CanvasHS\", \"fontSize\": 12, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"left\", \"bold\": true, \"italic\": false, \"underline\": true}}"
            it "can encode combine all properties" $ do
                shapeDec (Text (1,2) "CanvasHS" defaults{font="comic sans ms",size=10,alignment=AlignRight,bold=True,underline=True,italic=False}) `shouldBe` shapeTextDec "{\"type\": \"text\", \"data\": {\"x\": 1, \"y\": 2, \"fontFamily\": \"comic sans ms\", \"text\": \"CanvasHS\", \"fontSize\": 10, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}, \"align\": \"right\", \"bold\": true, \"italic\": false, \"underline\": true}}"
        describe "Events" $ do
            it "can add an event to a shape" $ do
                shapeDec (Event defaults{eventId="circle1", mouseClick=True} (Circle (1,2) 3)) `shouldBe` shapeTextDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"eventData\": {\"eventId\": \"circle1\", \"listen\": [\"mouseclick\"]}}"
            it "can add multiple events to a shape" $ do
                shapeDec (Event defaults{eventId="circle1", mouseDown=True, mouseClick=True, mouseUp=True, mouseDoubleClick=True, mouseDrag=True, mouseOver=True, mouseOut=True, scroll=True} (Circle (1,2) 3)) `shouldBe` shapeTextDec "{\"type\": \"circle\", \"data\": {\"x\": 1, \"y\": 2, \"radius\": 3, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"eventData\": {\"eventId\": \"circle1\", \"listen\": [\"mousedown\", \"mouseclick\", \"mouseup\", \"mousedoubleclick\", \"mousedrag\", \"mouseover\", \"mouseout\", \"scroll\"]}}"
            it "can add events to a container" $ do
                shapeDec (Event defaults{eventId="container1", mouseClick=True, mouseDrag=True} (Container 1 2 [(Circle (3,4) 5)])) `shouldBe` shapeTextDec "{\"type\": \"container\", \"data\": {\"x\": 0, \"y\": 0, \"width\": 1, \"height\": 2, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}, \"eventData\": {\"eventId\": \"container1\", \"listen\": [\"mouseclick\", \"mousedrag\"]}, \"children\": [{\"type\": \"circle\", \"data\": {\"x\": 3, \"y\": 4, \"radius\": 5, \"fill\": {\"r\":0,\"g\":0,\"b\":0,\"a\":1.0}}}]}"
    describe "Protocol.encode for Actions" $ do
        describe "Debugger" $ do
            it "can enable debugger" $ do
                actionDec [Debug True] `shouldBe` actionsTextDec "[{\"action\": \"debugger\", \"data\": {\"enabled\": true}}]"
            it "can disable debugger" $ do
                actionDec [Debug False] `shouldBe` actionsTextDec "[{\"action\": \"debugger\", \"data\": {\"enabled\": false}}]"
        describe "Drag'n'drop" $ do
            it "can enable dag'n'drop without multiple files" $ do
                actionDec [DragNDrop True False] `shouldBe` actionsTextDec "[{\"action\": \"acceptfiledragndrop\", \"data\": {\"enabled\": true, \"multiple\": false}}]"
            it "can enable dag'n'drop with multiple files" $ do
                actionDec [DragNDrop True True] `shouldBe` actionsTextDec "[{\"action\": \"acceptfiledragndrop\", \"data\": {\"enabled\": true, \"multiple\": true}}]"
            it "can disable drag'n'drop" $ do
                actionDec [DragNDrop False False] `shouldBe` actionsTextDec "[{\"action\": \"acceptfiledragndrop\", \"data\": {\"enabled\": false, \"multiple\": false}}]"
        describe "DisplayType" $ do
            it "can set the DisplayType to FixedSize" $ do
                actionDec [DisplayType (FixedSize 1 2)] `shouldBe` actionsTextDec "[{\"action\": \"windowdisplaytype\", \"data\": {\"type\": 0, \"width\": 1, \"height\": 2}}]"
            it "can set the DisplayType to FulWindow" $ do
                actionDec [DisplayType FullWindow] `shouldBe` actionsTextDec "[{\"action\": \"windowdisplaytype\", \"data\": {\"type\": 1}}]"
            it "can set the DisplayType to FullScreen" $ do
                actionDec [DisplayType FullScreen] `shouldBe` actionsTextDec "[{\"action\": \"windowdisplaytype\", \"data\": {\"type\": 2}}]"
        describe "Download" $ do
            it "can offer a file for download" $ do
                actionDec [Download "file.txt" "42"] `shouldBe` actionsTextDec "[{\"action\": \"download\", \"data\": {\"filecontents\": \"NDI=\", \"filename\": \"file.txt\"}}]"
        describe "RequestUpload" $ do
            it "can request uploading a single file" $ do
                actionDec [RequestUpload False] `shouldBe` actionsTextDec "[{\"action\": \"requestupload\", \"data\": {\"multiple\": false}}]"
            it "can request uploading multiple files" $ do
                actionDec [RequestUpload True] `shouldBe` actionsTextDec "[{\"action\": \"requestupload\", \"data\": {\"multiple\": true}}]"
        describe "Multiple actions" $ do
            it "can encode multiple actions" $ do
                actionDec [Debug True, DisplayType FullWindow] `shouldBe` actionsTextDec "[{\"action\": \"debugger\", \"data\": {\"enabled\": true}}, {\"action\": \"windowdisplaytype\", \"data\": {\"type\": 1}}]"

---------------------- HELPERS ----------------------

-- convenience functions to decodeJson
textDec :: T.Text -> Maybe Value
textDec = (decode . B.fromString . T.unpack) --decode is from Aeson

--textDec for when the text only holds a Shape
shapeTextDec :: T.Text -> Maybe Value
shapeTextDec t = textDec $ T.concat ["{\"shape\" : ", t, ", \"actions\":[]}"]

--textDec for when the text only holds an Action (Note actions should include "[]"!
actionsTextDec :: T.Text -> Maybe Value
actionsTextDec t = textDec $ T.concat ["{\"actions\": ", t, "}"]

dec :: Output -> Maybe Value
dec o = textDec $ encode o' --encode is from CanvasHs.Protocol
        where Out o' = o

shapeDec :: Shape -> Maybe Value
shapeDec = (dec . shape)

actionDec :: [Action] -> Maybe Value
actionDec acs = dec (Out (Nothing, acs))