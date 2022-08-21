{- LANGUAGE MagicHash -}

module MVector2 where

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
  let _sqrt = floor $ sqrt $ fromIntegral max
      sieve = runST (do
          msieve <- MV.replicate (max + 1) True
          MV.write msieve 0 False
          MV.write msieve 1 False
          let go i =
                if i > _sqrt
                  then pure ()
                  else do
                    isprime <- MV.unsafeRead msieve i
                    if isprime
                      then (
                        let go j =
                              if j > max
                                then pure ()
                                else MV.unsafeWrite msieve j False >> go (j + i)
                        in go (i + i)
                      )
                      else pure ()
                    go (i+1)
          go 2
          V.unsafeFreeze msieve
        )

      primes = runST (do
          mprimes <- MV.unsafeNew (max + 1)
          let go i pc = 
                if i > max
                  then pure pc
                  else do
                    let isprime = V.unsafeIndex sieve i
                    if isprime
                    then MV.unsafeWrite mprimes pc i >> go (i + 1) (pc + 1)
                    else go (i + 1) pc
          pcount <- go 2 0
          V.unsafeFreeze $ MV.unsafeSlice 0 pcount mprimes
        )
  in primes
