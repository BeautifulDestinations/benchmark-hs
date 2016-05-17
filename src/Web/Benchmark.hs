
{-# LANGUAGE JavaScriptFFI, CPP, OverloadedStrings, BangPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}


{-|

-}

module Web.Benchmark
    -- (

    -- )
    where

-- import Control.Monad(forM_, forever)
-- import Data.Monoid
--
import GHCJS.Types
-- import Data.JSString
--
-- import GHCJS.Foreign.Callback as CB
-- import qualified GHCJS.Foreign as F
-- import qualified JavaScript.Array as A
-- import qualified JavaScript.Object as O
-- import System.IO.Unsafe (unsafePerformIO)
-- import Data.IORef


newtype Suite = Suite JSVal

foreign import javascript safe "new h$benchmark.Benchmark.Suite"
  newSuite :: IO Suite

foreign import javascript safe "console.log($1)"
  debugLogSuite :: Suite -> IO ()



-- foreign import javascript unsafe "h$vdom.node($1,$2,$3,$4,$5)"
--   primNode :: JSString -> JSVal -> JSVal -> JSVal -> JSVal -> Node
--
-- foreign import javascript unsafe "h$vdom.node($1,$2,$3,undefined,$4)"
--   primNode_4th_argument_undefined :: JSString -> JSVal -> JSVal -> JSString -> Node
--
-- foreign import javascript unsafe "h$vdom.staticNode($1, $2, $3, $4, $5)"
--   primStNode :: JSString -> JSVal -> JSVal -> JSVal -> JSVal -> Node
