
{-# LANGUAGE JavaScriptFFI, CPP, OverloadedStrings, BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


{-|
Bindings to benchmark.js

- Treats Benchmark type as immutable
-}

module Web.Benchmark
    ( SuiteM
    , add
    , addWithPrepare
    , onCycle
    , onComplete
    , run
    , liftIO
    -- , liftS
    , Suite
    -- , newSuite
    -- , addSuite
    -- , onCycleSuite
    -- , onCompleteSuite
    -- , runSuite
    , benchmarks
    , Benchmark
    , name
    , hz
    , maxTime
    , minTime
    , cycles
    , defer
    , delay
    -- * Debug
    , debugLogSuite
    , debugLogBenchmark

    )
    where

import GHCJS.Types
import Data.JSString
import GHCJS.Foreign.Callback
import JavaScript.Array (JSArray, toList)
import Control.Monad.State
import Control.Monad.IO.Class(liftIO)

{-| Use this to build up a test suite-}
newtype SuiteM a = S (StateT Suite IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState Suite)

{-| Add a benchmark to the suite. -}
add :: JSString -> IO () -> SuiteM ()
add n k = do
  s <- get
  s <- liftIO $ addSuite n k s
  put s

{-| Add a benchmark to the suite.
    Run outer IO action as a prepare step.
    Inner IO action is what is actually measured.
-}
addWithPrepare :: JSString -> IO (IO ()) -> SuiteM ()
addWithPrepare n k = do
  k2 <- liftIO k
  add n k2

{-| Called when a benchmark has been completed. -}
onCycle :: (Benchmark -> IO ()) -> SuiteM ()
onCycle k = do
  s <- get
  s <- liftIO $ onCycleSuite k s
  put s

{-| Called when the whole suite has been completed. -}
onComplete :: (Suite -> IO ()) -> SuiteM ()
onComplete k = do
  s <- get
  s <- liftIO $ onCompleteSuite k s
  put s

{-| Run benchmarks and invoke handlers as you go. -}
run :: SuiteM a -> IO a
run (S k) =  do
  s <- newSuite
  (a, s) <- runStateT k s
  runSuite s
  pure a


newtype Suite = Suite JSVal
newtype Benchmark = Benchmark JSVal

foreign import javascript unsafe "$1.cycles"
  cycles :: Benchmark -> Int
foreign import javascript unsafe "$1.defer"
  defer :: Benchmark -> Bool
foreign import javascript unsafe "$1.delay"
  delay :: Benchmark -> Double

foreign import javascript unsafe "$1.maxTime"
  maxTime :: Benchmark -> Double
foreign import javascript unsafe "$1.minTime"
  minTime :: Benchmark -> Double

foreign import javascript unsafe "$1.hz"
  hz :: Benchmark -> Double
foreign import javascript unsafe "$1.name"
  name :: Benchmark -> JSString



benchmarks :: Suite -> IO [Benchmark]
benchmarks s = do
  bs <- benchmarks' s
  pure $ fmap Benchmark $ toList bs

foreign import javascript safe "h$benchmark.lodash.toArray($1)"
  benchmarks' :: Suite -> IO JSArray

foreign import javascript safe "new h$benchmark.Benchmark.Suite"
  newSuite :: IO Suite

foreign import javascript safe "console.log($1)"
  debugLogSuite :: Suite -> IO ()
foreign import javascript safe "console.log($1)"
  debugLogBenchmark :: Benchmark -> IO ()

addSuite :: JSString -> IO () -> Suite -> IO Suite
addSuite n k s = do
  k2 <- syncCallback ThrowWouldBlock k
  addSuite' n k2 s

foreign import javascript safe "$3.add($1, $2)"
  addSuite' :: JSString -> Callback (IO ()) -> Suite -> IO Suite


-- TODO possibly unsafe
onCycleSuite :: (Benchmark -> IO ()) -> Suite -> IO Suite
onCycleSuite k s = do
  k2 <- syncCallback1 ThrowWouldBlock (k . Benchmark)
  onCycleSuite' k2 s

foreign import javascript safe "$2.on('cycle', function(event) { $1(event.target) })"
  onCycleSuite' :: Callback (JSVal -> IO ()) -> Suite -> IO Suite


onCompleteSuite :: (Suite -> IO ()) -> Suite -> IO Suite
onCompleteSuite k s = do
  k2 <- syncCallback1 ThrowWouldBlock (k . Suite)
  onCompleteSuite' k2 s

foreign import javascript safe "$2.on('complete', function(){ $1(this) })"
  onCompleteSuite' :: Callback (JSVal -> IO ()) -> Suite -> IO Suite

foreign import javascript safe "$1.run()"
  runSuite :: Suite -> IO ()
