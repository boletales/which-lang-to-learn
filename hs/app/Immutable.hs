-- 筆者の環境で20~25秒

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Immutable where

import Lib
import qualified Data.Vector.Unboxed as V
import Data.Foldable
import Control.Monad
import GHC.Float

num :: Int
num = 100000000

main :: IO ()
main = do
  putStrLn "start"
  let ps = generatePrimes num
  print $ V.last ps
  putStrLn "end"

generatePrimes :: Int -> V.Vector Int
generatePrimes max =
  let sieve = 
        foldl (\v i -> 
            if V.unsafeIndex v i
            --then V.unsafeUpdate_ v (V.generate (max `div` i - i + 1) (\j -> i * (i + j))) (V.replicate (max `div` i - i  + 1) False)
            then v V.// ((,False) <$> [i*i,i*i+i..max])
            else v
          ) (V.replicate (max+1) True V.// [(0,False),(1,False)]) [2..(double2Int $ sqrt $ fromIntegral max)]

  in V.filter (V.unsafeIndex sieve) (V.generate (max+1) id)
