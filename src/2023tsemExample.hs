{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Monad

waitTime :: Int
waitTime = 2 * 1000 * 1000

afterDelay :: Int -> IO () -> IO ()
afterDelay n action = threadDelay n >> action

-- run action when lock is free
runAction :: TSem -> IO () -> IO ()
runAction sem action =
  void $
    forkIO $
      atomically (waitTSem sem) >> action           -- wait for lock

-- signal free lock
signalFree :: TSem -> IO ()
signalFree sem = atomically $ signalTSem sem        --

test :: IO ()
test = do
  -- at most 3 processes at once
  sem <- atomically $ newTSem 3

  -- parallel
  runAction sem $ afterDelay waitTime $ putStrLn "a"
  runAction sem $ afterDelay waitTime $ putStrLn "b"
  runAction sem $ afterDelay waitTime $ putStrLn "c" >> signalFree sem

  -- sequential
  runAction sem $ afterDelay waitTime $ putStrLn "d" >> signalFree sem
  runAction sem $ afterDelay waitTime $ putStrLn "e" >> signalFree sem
  runAction sem $ afterDelay waitTime $ putStrLn "f" >> signalFree sem >> signalFree sem >> signalFree sem

  -- parallel
  runAction sem $ afterDelay waitTime $ putStrLn "g" 
  runAction sem $ afterDelay waitTime $ putStrLn "h"
  runAction sem $ afterDelay waitTime $ putStrLn "i"

  -- never run
  runAction sem $ afterDelay waitTime $ putStrLn "z"

testSequential :: IO ()
testSequential = do
  sem <- atomically $ newTSem 1
  runAction sem $ afterDelay waitTime $ putStrLn "a" >> signalFree sem
  runAction sem $ afterDelay waitTime $ putStrLn "b" >> signalFree sem
  runAction sem $ afterDelay waitTime $ putStrLn "c" >> signalFree sem
  pure ()
