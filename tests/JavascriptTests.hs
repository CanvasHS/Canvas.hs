import Prelude
import System.Process
import System.Exit

main = runTestScripts

runTestScripts = do
	putStrLn "Running javascript tests "
	putStrLn "Phantomjs is required http://phantomjs.org (make sure it is in the PATH) "
	executeShellCommand "phantomjs -v"
	executeShellCommand "phantomjs tests/run-jasmine.js canvashs-client/tests/test.html"
	putStrLn "Finished test script "

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = error ("cmd: " ++ cmd ++ " failure code " ++ show n)