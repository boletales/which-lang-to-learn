module MVector where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Control.Loop

num :: Int
num = 100000000

main :: IO ()
main = do
  putStrLn "start"
  let ps = generatePrimes num
  putStrLn $ "found " <> (show (V.length ps)) <> " primes"
  putStrLn $ show (V.last ps)
  putStrLn "end"

generatePrimes :: Int -> V.Vector Int
generatePrimes max =
  let sieve = runST (do
          msieve <- MV.replicate (max+1) True
          MV.write msieve 0 False
          MV.write msieve 1 False
          
          numLoop 2 (floor $ sqrt $ fromIntegral max) $ \i -> do
            isprime <- MV.unsafeRead msieve i
            when isprime (numLoop i (max `div` i) (\j -> MV.unsafeWrite msieve (i*j) False))
          
          V.unsafeFreeze msieve
        )

      primes = runST (do
          mprimes <- MV.unsafeNew (max+1)
            
          pcount <- foldM (\pcount i -> do
            let isprime = V.unsafeIndex sieve i
            if isprime
            then (do
              MV.unsafeWrite mprimes pcount i
              pure (pcount+1)
              )
            else pure pcount
            ) 0 [2..max]
          V.unsafeFreeze $ MV.unsafeSlice 0 pcount mprimes
        )
  in primes
