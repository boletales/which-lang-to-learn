-- Repaは行列演算みたいなのには非常に向いているが、エラトステネスのふるいみたいな1要素ずつ書き換えるやつには向いていないらしい
-- 筆者の環境で一分強

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Repa where

import Lib
import Data.Array.Repa as Repa
import Data.Foldable
import Control.Monad
import GHC.Float
import Data.Functor.Identity

num :: Int
num = 100000000

main :: IO ()
main = do
  putStrLn "start"
  let ps = generatePrimes num
  print $ Repa.index ps (Z :. (head (Repa.listOfShape (Repa.extent ps)) -1))
  putStrLn "end"

generatePrimes :: Int -> Array U DIM1 Int
generatePrimes max =
  let sieve = 
        foldl (\a i -> 
            if Repa.index a (Z :. i)
            --then V.unsafeUpdate_ v (V.generate (max `div` i - i + 1) (\j -> i * (i + j))) (V.replicate (max `div` i - i  + 1) False)
            then Repa.traverse a id (\lookup ix@(Z :. j) -> if j `mod` i == 0 then False else lookup ix)
            else a
          ) (Repa.fromFunction (Z :. max + 1) (const True)) [2..(double2Int $ sqrt $ fromIntegral max)]

  in runIdentity $ Repa.selectP (\i -> Repa.index sieve (Z :. i)) id (max+1)
