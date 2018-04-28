{-# language LambdaCase #-}

import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import Data.Foldable
import Data.IORef
import Gauge
import GHC.Event
import System.Clock
import System.Environment
import System.IO.Unsafe
import System.Random

import qualified Data.TimerWheel as TimerWheel

main :: IO ()
main =
  getArgs >>= \case
    ["wheel", n, m] -> timerWheelMain (read n) (read m)
    ["ghc", n, m] -> ghcMain (read n) (read m)

timerWheelMain :: Int -> Int -> IO ()
timerWheelMain n m = do
  wheel <- TimerWheel.new (2^(16::Int)) (1/10)

  done <- newEmptyMVar :: IO (MVar ())

  replicateM_ n . forkIO $ do
    timers <-
      replicateM m $ do
        s:ss <- readIORef doublesRef
        writeIORef doublesRef ss
        TimerWheel.register (realToFrac s) (pure ()) wheel
    for_ timers TimerWheel.cancel
    putMVar done ()

  replicateM_ n (takeMVar done)

ghcMain :: Int -> Int -> IO ()
ghcMain n m = do
  mgr <- getSystemTimerManager

  done <- newEmptyMVar :: IO (MVar ())

  replicateM_ n . forkIO $ do
    timers <-
      replicateM m $ do
        s:ss <- readIORef intsRef
        writeIORef intsRef ss
        registerTimeout mgr s (pure ())
    for_ timers (unregisterTimeout mgr)
    putMVar done ()

  replicateM_ n (takeMVar done)

doublesRef :: IORef [Double]
doublesRef =
  -- unsafePerformIO (newIORef (randomRs (6553.6, 13107.2) (mkStdGen 1)))
  unsafePerformIO (newIORef (randomRs (5, 10) (mkStdGen 1)))
{-# NOINLINE doublesRef #-}

intsRef :: IORef [Int]
intsRef =
  unsafePerformIO (newIORef (randomRs (5000000, 10000000) (mkStdGen 1)))
{-# NOINLINE intsRef #-}
