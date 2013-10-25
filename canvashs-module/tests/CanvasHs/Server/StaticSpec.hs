module CanvasHs.Server.StaticSpec (main, spec) where

import Test.Hspec
import CanvasHs.Server.Static

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "get directories" $ do
    it "returns the list of directories in a cetain path" $ do
      dirs <- getDirectories "canvashs-client"
      ("canvashs-client/js" `elem` dirs) `shouldBe` True