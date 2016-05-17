
{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Control.Monad(forM_)
import Data.String(fromString)
import Data.Monoid ((<>))

import Web.Benchmark

main = run $ do
  liftIO $ print "Starting benchmarks..."
  add "fast" fast
  add "slow" slow
  onCycle $ \b -> do
    print $ " Completed: " <> name b <> ": " <> fromString (show $ hz b)
  onComplete $ \s -> do
    print "Done"
    bs <- benchmarks s
    forM_ bs $ \b -> do
      print $ " Result: " <> name b <> ": " <> fromString (show $ hz b)

fast, slow :: IO ()
fast = do
  let !x = 1 + 2 :: Int
  return ()

slow = do
  let !x = length (take 100 [1..])
  return ()
