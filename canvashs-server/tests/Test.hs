import Data.Char
import Data.List
import Test.QuickCheck
import Text.Printf
 
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests
 
-- ("testname", quickCheck function)
tests  = [("testname", quickCheck (True == id True))]