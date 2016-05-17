
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Control.Monad(forM_)
import Data.String(fromString)
import Data.Monoid ((<>))
import Web.Benchmark

main = do
  print "Benchmark tests!!"
  suite <- newSuite

  suite <- add "fast" fast suite
  suite <- add "slow" slow suite

  suite2 <- flip onComplete suite $ \s -> do
    print "Done"
    -- debugLogSuite s
    bs <- benchmarks s
    -- mapM_ debugLogBenchmark bs
    forM_ bs $ \b -> do
      print $ " " <> name b <> ": " <> fromString (show $ hz b)

  print "Start"
  run suite2
  print "Stop"
  return ()

fast, slow :: IO ()
fast = do
  let !x = 1 + 2 :: Int
  return ()

slow = do
  let !x = length (take 100 [1..])
  return ()
