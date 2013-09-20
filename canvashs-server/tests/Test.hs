import Data.Char
import Data.List
import Test.Hspec
import Test.QuickCheck
import Text.Printf
import Control.Exception (evaluate)

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
-- ("testname", quickCheck function)
tests  = [("testname", quickCheck (True == id True))]
