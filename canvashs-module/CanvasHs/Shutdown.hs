{- |
    Shutdown can be used to execute functions when exiting the program
    This can be very usefull when threads need to be stopped on exit.
    addEnd will add a function to the list of functions to be executed 
    shutdown will block while the functions added by addEnd are executed.
-}
module CanvasHs.Shutdown
(
    addEnd,
    shutdown
)
where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Debug.Trace (traceShow)

-- | unsafePerformIO-hack function to manage thread completion
ends :: IORef ([IO ()])
{-# NOINLINE ends #-}
ends = unsafePerformIO (newIORef [])

-- | adds a function which should be executed when the shutdown function is called
addEnd :: IO () -> IO ()
addEnd a = atomicModifyIORef ends (\es -> (a:es, ()))

-- | shutdown calls all functions added by addEnd and will block untill all of them have finished.
--   executes all function sequentially in the current thread
shutdown :: IO ()
shutdown = readIORef ends >>= shutdown'
    where 
        shutdown' :: [IO()] -> IO ()
        shutdown' []     = return ()
        shutdown' (e:es) = traceShow (length es + 1) $ e `seq` shutdown' es 