import Prelude
import System.Process
import System.Exit

main = cloneRepos

cloneRepos
  = do putStrLn "Install Test scripts for Javascript "
       putStrLn "Git is required http://git-scm.com/ (make sure it is in the PATH) "
       executeShellCommand "git -v"
       executeShellCommand "git clone https://github.com/pivotal/jasmine.git lib/jasmine -b v1.3.1"
       executeShellCommand "git clone https://github.com/HumbleSoftware/js-imagediff.git lib/js-imagediff -b v1.0.4"
       executeShellCommand "git clone https://github.com/alex-seville/blanket.git lib/blanket"
       putStrLn "Finished test install script"

executeShellCommand cmd   = putStrLn ("EXEC: " ++ cmd) >> system cmd >>= check
  where 
    check (ExitSuccess)   = return ()
    check (ExitFailure n) = putStrLn ("cmd: " ++ cmd ++ " failure code " ++ show n)