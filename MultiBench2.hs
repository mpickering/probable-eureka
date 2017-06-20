{-# language FlexibleContexts, BangPatterns #-}
module Main (main) where

-- | Benchmarks for various effect system implementations

import Control.Monad
import Criterion.Main
import System.Random

import Control.Monad.State.Strict
import Control.Monad.Reader

-- Use only state, lift variable number of effects over/under
--------------------------------------------------------------------------------

test1mtl :: MonadState Int m => Int -> m Int
test1mtl n = foldM f 1 [0..n] where
  f acc x | x `rem` 5 == 0 = do
              s <- get
              put $! (s + 1)
              pure $! max acc x
          | otherwise = pure $! max acc x

main = do

  -- Used to definitively disable bench argument inlining
  !n <- randomRIO (1000000, 1000000) :: IO Int
  !m <- randomRIO (0, 0) :: IO Int

  let runRT = (`runReaderT`  (m :: Int))
  let runS  = (`runState`    (m :: Int))
  defaultMain [

     bgroup "test1" [
       bgroup "MTL" [
         bench "SR"    $ whnf (runS . runRT . test1mtl) n,
         -- Comment out this line and the benchmark is 6x faster with GHC
         -- 8.2.1 but not with 8.0.2
         bench "SR"    $ whnf (runS . runRT . test1mtl) n,
         -- They should all be as fast as this case
         bench "SRR"   $ whnf (runS . runRT . runRT . test1mtl) n
         ] ]  ]
