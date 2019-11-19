module MersenneTwister( extractNumber
                      , getMtInts
                      , getMtIntsState
                      , mtInt
                      , MTState
                      , seedMt
                      ) where

import Control.Monad.State
import Data.Bits((.&.), complement, shift, shiftR, xor)
import Data.Word(Word8)
import Debug.Trace

import qualified Lib

-- Coefficients
w = 32
n = 624
m = 397
r = 31

a = 0x9908B0DF

u = 11
d = 0xFFFFFFFF

s = 7
b = 0x9D2C5680

t = 15
c = 0xEFC60000

l = 18

f = 1812433253
----------------

lowestWBits :: Int -> Int
lowestWBits x = x .&. ((1 `shift` w) - 1)

lowerMask = (1 `shift` r) - 1 
upperMask = lowestWBits (complement lowerMask)

type MTState = (Int, [Int])

seedMtHelper :: Int -> Int -> Int -> [Int]
seedMtHelper idx stop prev
  | idx == stop = []
  | otherwise = thisValue : (seedMtHelper (idx+1) stop thisValue)
    where
        thisValue = lowestWBits (f * (prev `xor` (prev `shiftR` (w-2))) + idx)

seedMt :: Int -> MTState
seedMt seed = (n, seed:mtState)
    where 
        mtState = seedMtHelper 1 n seed

twistHelper :: Int -> Int -> [Int] -> [Int]
twistHelper idx stop prev
  | idx == stop = prev
  | otherwise = twistHelper (idx+1) stop cur
    where 
        cur = Lib.replaceNth idx prev thisValue
        thisValue = (prev !! ((idx + m) `mod` n)) `xor` xA'

        xA' = if (x `mod` 2) /= 0 then xA `xor` a else xA
        xA = x `shiftR` 1

        x = ((prev !! idx) .&. upperMask) 
          + ((prev !! ((idx+1) `mod` n)) .&. lowerMask)

twist :: MTState -> MTState
twist (_, prev) = (0, twistHelper 0 n prev)

extractNumber :: MTState -> (Int, MTState)
extractNumber (idx, prev) = (r, (idx'+1, cur))
    where
        (idx', cur) = if idx == n 
                         then twist (idx, prev) 
                         else (idx, prev)
        y = cur !! idx'
        y' = y `xor` ((y `shiftR` u) .&. d)
        y'' = y' `xor` ((y' `shift` s) .&. b)
        y''' = y'' `xor` ((y'' `shift` t) .&. c)
        y'''' = y''' `xor` (y''' `shiftR` l)
        r = lowestWBits y''''

mtInt :: State MTState Int
mtInt = state  $ \s -> extractNumber s

getMtIntsState :: Int -> MTState -> ([Int], MTState)
getMtIntsState n mt = runState (sequence $ replicate n mtInt) mt

getMtInts :: Int -> MTState -> [Int]
getMtInts n mt = fst (getMtIntsState n mt) 
