module CanvasHs.Protocol.InputSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck
import CanvasHs.Protocol.Input
import CanvasHs.Data
import CanvasHs.Protocol (decode)
import Data.Text
 
--instance Arbitrary Char where
--    arbitrary     = choose ('\32', '\128')

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "mousedown event" $ do
            it "can decode an mousedown event" $ do
                (decode $ pack $ "{\"event\":\"mousedown\", \"eventData\":{\"id\": \"myAwesomeShape\", \"x\": 150, \"y\":200}}") `shouldBe` (MouseDown (150, 200) "myAwesomeShape")
            it "can decode an mouseclick event" $ do
                (decode $ pack $ "{\"event\":\"mouseclick\", \"eventData\":{\"id\": \"myAwesomeShape\", \"x\": 150, \"y\":200}}") `shouldBe` (MouseClick (150, 200) "myAwesomeShape")


--            it "returns an *arbitrary* mousedown event" $
--                property $ \x y eid -> (decode $ buildMouse "mousedown" x y eid) == (MouseDown (x, y) eid)
            it "can decode an mouseup event" $ do
                (decode $ pack $ "{\"event\":\"mouseup\", \"eventData\":{\"id\": \"myAwesomeShape\", \"x\": 150, \"y\":200}}") `shouldBe` (MouseUp (150, 200) "myAwesomeShape")
            it "can decode an mouseenter event" $ do
                (decode $ pack $ "{\"event\":\"mouseenter\", \"eventData\":{\"id\": \"myAwesomeShape\", \"x\": 150, \"y\":200}}") `shouldBe` (MouseEnter (150, 200) "myAwesomeShape")
            it "can decode an mouseleave event" $ do
                (decode $ pack $ "{\"event\":\"mouseleave\", \"eventData\":{\"id\": \"myAwesomeShape\", \"x\": 150, \"y\":200}}") `shouldBe` (MouseLeave (150, 200) "myAwesomeShape")
            it "can decode an keydown event" $ do
                (decode $ pack $ "{\"event\":\"keydown\", \"eventData\":{\"key\": \"c\", \"control\": true, \"alt\": true, \"shift\": true, \"super\": true}}") `shouldBe` (KeyDown 'c' [Ctrl, Alt, Shift, Super])
            it "can decode an keyclick event" $ do
                (decode $ pack $ "{\"event\":\"keyclick\", \"eventData\":{\"key\": \"c\", \"control\": true, \"alt\": true, \"shift\": true, \"super\": true}}") `shouldBe` (KeyClick 'c' [Ctrl, Alt, Shift, Super])
            it "can decode an keyup event" $ do
                (decode $ pack $ "{\"event\":\"keyup\", \"eventData\":{\"key\": \"c\", \"control\": true, \"alt\": true, \"shift\": true, \"super\": true}}") `shouldBe` (KeyUp 'c' [Ctrl, Alt, Shift, Super])
            it "can decode a scroll event" $ do
                (decode $ pack $ "{\"event\":\"scroll\", \"eventData\":{\"xdelta\": 10, \"ydelta\": 10}}") `shouldBe` (Scroll 10 10)


buildMouse :: String -> Int -> Int -> String -> Text
buildMouse ty x y eid = pack $ "{\"event\":\"" ++ ty ++ "\", \"eventData\":{\"id\": \"" ++ (eid) ++ "\", \"x\": " ++ (show x) ++ ", \"y\": " ++ (show y) ++ "}}"
