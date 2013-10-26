module CanvasHs.Protocol.OutputSpec (main,spec) where

import Test.Hspec
import Test.QuickCheck
import CanvasHs.Data
import Data.Text
import CanvasHs.Protocol (encode)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
        describe "circle" $ do
            it "can translate a circle" $ do
                (encode $ Circle (100, 100) 10) `shouldBe` (pack $ "{{\"type\":\"circle\", \"data\":{\"x\":100, \"y\":100, \"radius\":10}}")
