import Control.DeepSeq
import Control.Monad
import Data.IORef
import Gauge
import GHC.Event
import System.Clock
import System.IO.Unsafe
import System.Random

import qualified Data.TimerWheel as TimerWheel

main :: IO ()
main =
  defaultMain
    [
      -- bench "getTime Monotonic" (whnfIO (getTime Monotonic))

    -- , env (newIORef ())
    --     (\ref -> bench "readIORef" (whnfIO (readIORef ref)))

    -- ,

      bench "register"
        (perBatchEnvWithCleanup
          (\_ ->
            (,)
              <$> TimerWheel.new 10 (1/10)
              <*> makeDoubles)
          (\_ (wheel, _) -> TimerWheel.stop wheel)
          (\(wheel, doublesRef) -> do
            s:ss <- readIORef doublesRef
            writeIORef doublesRef ss
            TimerWheel.register_ (realToFrac s) (pure ()) wheel))

    , env
        ((,)
          <$> getSystemTimerManager
          <*> makeInts)
        (\ ~(mgr, intsRef) ->
          bench "GHC.Event.registerTimeout" . whnfIO $ do
            s:ss <- readIORef intsRef
            writeIORef intsRef ss
            void (registerTimeout mgr s (pure ())))

    ]

makeDoubles :: IO (IORef [Double])
makeDoubles =
  newIORef (take 10000000 (randomRs (0.1, 10) (mkStdGen 1)))

makeInts :: IO (IORef [Int])
makeInts =
  newIORef (take 10000000 (randomRs (100000, 10000000) (mkStdGen 1)))

instance NFData TimerManager where
  rnf _ = ()
