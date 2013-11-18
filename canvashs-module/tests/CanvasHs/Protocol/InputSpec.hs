module CanvasHs.Protocol.InputSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck
import CanvasHs.Data
import CanvasHs.Protocol (decode)
import Data.Text
import Text.Printf

-- | A JSON valid string
newtype JSONString = JSONString String

-- | Instance for QuickChecks arbitrary
instance Arbitrary JSONString where
    arbitrary     = do
        len <- choose(1,20)
        arb <- (vectorOf len $ oneof [choose('a', 'z'), choose('A', 'Z')])
        return $ JSONString arb

-- | A JSON valid character
newtype JSONChar = JSONChar Char

-- | Instance for QuickChecks arbitrary
instance Arbitrary JSONChar where
    arbitrary = do
        char <- oneof [choose('a', 'z'), choose('A', 'Z')]
        return $ JSONChar char

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "mousedown event" $ do
            it "can decode a mousedown event" $ do
                (decode $ pack $ buildMousePF "mousedown" 150 200 "myAwesomeShape") `shouldBe` (MouseDown (150, 200) "myAwesomeShape")
            it "can decode *arbitrary* mousedown event" $ do
                JSONString eid <- arbitrary
                property $ \x y -> (decode $ pack $ buildMousePF "mousedown" x y eid) == (MouseDown (x, y) eid)
        describe "mouseclick event" $ do
            it "can decode a mouseclick event" $ do
                (decode $ pack $ buildMousePF "mouseclick" 150 200 "myAwesomeShape") `shouldBe` (MouseClick (150, 200) "myAwesomeShape")
            it "can decode *arbitrary* mouseclick event" $ do
                JSONString eid <- arbitrary
                property $ \x y -> (decode $ pack $ buildMousePF "mouseclick" x y eid) == (MouseClick (x, y) eid)
        describe "mouseup event" $ do
            it "can decode a mouseup event" $ do
                (decode $ pack $ buildMousePF "mouseup" 150 200 "myAwesomeShape") `shouldBe` (MouseUp (150, 200) "myAwesomeShape")
            it "can decode *arbitrary* mouseup event" $ do
                JSONString eid <- arbitrary
                property $ \x y -> (decode $ pack $ buildMousePF "mouseup" x y eid) == (MouseUp (x, y) eid)
        describe "mouseover event" $ do
            it "can decode a mouseover event" $ do
                (decode $ pack $ buildMousePF "mouseover" 150 200 "myAwesomeShape") `shouldBe` (MouseOver (150, 200) "myAwesomeShape")
            it "can decode *arbitrary* mouseover event" $ do
                JSONString eid <- arbitrary
                property $ \x y -> (decode $ pack $ buildMousePF "mouseover" x y eid) == (MouseOver (x, y) eid)
        describe "mouseout event" $ do
            it "can decode a mouseout event" $ do
                (decode $ pack $ buildMousePF "mouseout" 150 200 "myAwesomeShape") `shouldBe` (MouseOut (150, 200) "myAwesomeShape")
            it "can decode *arbitrary* mouseout event" $ do
                JSONString eid <- arbitrary
                property $ \x y -> (decode $ pack $ buildMousePF "mouseout" x y eid) == (MouseOut (x, y) eid)
        describe "keydown event" $ do
            it "can decode a keydown event" $ do
                (decode $ pack $ buildKeyPF "keydown" 'c' True True True) `shouldBe` (KeyDown 'c' [Ctrl, Alt, Shift])
            it "can decode *arbitrary* keydown event" $ do
                JSONChar key <- arbitrary
                property $ \control alt shift -> (decode $ pack $ buildKeyPF "keydown" key control alt shift) == (KeyDown key $ makeModifiers control alt shift)
        describe "keyclick event" $ do
            it "can decode a keyclick event" $ do
                (decode $ pack $ buildKeyPF "keyclick" 'c' True True True) `shouldBe` (KeyClick 'c' [Ctrl, Alt, Shift])
            it "can decode *arbitrary* keyclick event" $ do
                JSONChar key <- arbitrary
                property $ \control alt shift -> (decode $ pack $ buildKeyPF "keyclick" key control alt shift) == (KeyClick key $ makeModifiers control alt shift)
        describe "keyup event" $ do
            it "can decode a keyup event" $ do
                (decode $ pack $ buildKeyPF "keyup" 'c' True True True) `shouldBe` (KeyUp 'c' [Ctrl, Alt, Shift])
            it "can decode *arbitrary* keyup event" $ do
                JSONChar key <- arbitrary
                property $ \control alt shift -> (decode $ pack $ buildKeyPF "keyup" key control alt shift) == (KeyUp key $ makeModifiers control alt shift)
        describe "scroll event" $ do
            it "can decode a scroll event" $ do
                (decode $ pack $ "{\"event\":\"scroll\", \"data\":{\"xdelta\": 10, \"ydelta\": 10}}") `shouldBe` (Scroll 10 10)
            it "can decond an *arbitrary* scroll event" $ do
                property $ \xdiff ydiff -> (decode $ pack $ "{\"event\":\"scroll\", \"data\":{\"xdelta\":" ++ (show xdiff) ++ ", \"ydelta\": " ++ (show ydiff) ++ "}}") `shouldBe` (Scroll xdiff ydiff)


-- | Convenience function for building json mouse event strings
buildMousePF :: String -> Int -> Int -> String -> String
buildMousePF event x y eid = 
    printf "{\"event\":\"%s\", \"data\":{\"id\": \"%s\", \"x\": %d , \"y\": %d }}" event eid x y

-- | Convenience function for building json key event strings
buildKeyPF :: String -> Char -> Bool -> Bool -> Bool -> String
buildKeyPF event key control alt shift =
    printf "{\"event\":\"%s\", \"data\":{\"key\": \"%c\", \"control\": %s, \"alt\": %s, \"shift\": %s}}" event key (if control then "true" else "false") (if alt then "true" else "false") (if shift then "true" else "false")

-- | This function is exactly what is used to construct the modifiers array
-- | but it is not exported from the import module and therefore it is copied here
makeModifiers :: Bool -> Bool -> Bool -> [Modifier]
makeModifiers ctrl alt shift = 
    (if ctrl then [Ctrl] else []) ++ 
    (if alt then [Alt] else []) ++
    (if shift then [Shift] else [])
