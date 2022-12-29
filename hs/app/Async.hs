module Async where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Data.STRef
import Control.Loop
import Control.Concurrent.Async
import Data.Bits 

num :: Int
num = 100000000

main :: IO ()
main = do
  putStrLn "start"
  ps <- generatePrimes num
  putStrLn $ "found " <> (show (V.length ps)) <> " primes"
  putStrLn $ show (V.last ps)
  putStrLn "end"

generatePrimes :: Int -> IO (V.Vector Int)
generatePrimes max = do
    msieve <- MV.replicate (max+1) True
    MV.write msieve 0 False
    MV.write msieve 1 False
    
    numLoop 1 (ceiling $ logBase 2 $ sqrt $ fromIntegral max) $ \l -> (
        forConcurrently_ [(shift 1 l) .. (min (shift 2 l) $ ceiling $ sqrt $ fromIntegral max)]  $ \i -> do
          isprime <- MV.unsafeRead msieve i
          when isprime (numLoop i (max `div` i) (\j -> MV.unsafeWrite msieve (i*j) False))
      )

    mprimes <- MV.unsafeNew (max+1)
    
    pcount <- numLoopState 2 max 0 (\pcount i -> do
      isprime <- MV.unsafeRead msieve i
      if isprime
      then (do
        MV.unsafeWrite mprimes pcount i
        pure (pcount+1)
        )
      else pure pcount
      )
    
    V.unsafeFreeze $ MV.unsafeSlice 0 pcount mprimes
