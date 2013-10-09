import Prelude
import System.Process
import System.Exit

main = cloneRepos

cloneRepos
  = do putStrLn "Install Test scripts for Javascript "
       executeShellCommand "git clone https://github.com/pivotal/jasmine.git lib/jasmine -b v1.3.1"
       executeShellCommand "git clone https://github.com/HumbleSoftware/js-imagediff.git lib/js-imagediff -b v1.0.4"
       putStrLn "Finished test install script"

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = putStrLn ("cmd: " ++ cmd ++ " failure code " ++ show n)