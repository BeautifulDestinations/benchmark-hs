
{-# LANGUAGE JavaScriptFFI, CPP, OverloadedStrings, BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


{-|

-}

module Web.Benchmark
    ( Suite
    , newSuite
    , add
    , onComplete
    , run
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

-- import Control.Monad(forM_, forever)
-- import Data.Monoid
--
import GHCJS.Types
import Data.JSString
import GHCJS.Foreign.Callback
import JavaScript.Array (JSArray, toList)
--
-- import GHCJS.Foreign.Callback as CB
-- import qualified GHCJS.Foreign as F
-- import qualified JavaScript.Array as A
-- import qualified JavaScript.Object as O
-- import System.IO.Unsafe (unsafePerformIO)
-- import Data.IORef


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

add :: JSString -> IO () -> Suite -> IO Suite
add n k s = do
  k2 <- syncCallback ThrowWouldBlock k
  add' n k2 s

foreign import javascript safe "$3.add($1, $2)"
  add' :: JSString -> Callback (IO ()) -> Suite -> IO Suite

onComplete :: (Suite -> IO ()) -> Suite -> IO Suite
onComplete k s = do
  k2 <- syncCallback1 ThrowWouldBlock (k . Suite)
  onComplete' k2 s

foreign import javascript safe "$2.on('complete', function(){ $1(this) })"
  onComplete' :: Callback (JSVal -> IO ()) -> Suite -> IO Suite

foreign import javascript safe "$1.run()"
  run :: Suite -> IO ()



-- foreign import javascript unsafe "h$vdom.node($1,$2,$3,$4,$5)"
--   primNode :: JSString -> JSVal -> JSVal -> JSVal -> JSVal -> Node
--
-- foreign import javascript unsafe "h$vdom.node($1,$2,$3,undefined,$4)"
--   primNode_4th_argument_undefined :: JSString -> JSVal -> JSVal -> JSString -> Node
--
-- foreign import javascript unsafe "h$vdom.staticNode($1, $2, $3, $4, $5)"
--   primStNode :: JSString -> JSVal -> JSVal -> JSVal -> JSVal -> Node
