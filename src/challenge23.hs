import Data.Bits
import Data.Char(intToDigit)
import Debug.Trace
import Numeric(showIntAtBase)
import Text.Printf


import MersenneTwister(mtInt, MTState, seedMt)

showBits :: Int -> String
showBits i = showIntAtBase 2 intToDigit i ""

n = 624


u = 11
d = 0xFFFFFFFF

s = 7
b = 0x9D2C5680

t = 15
c = 0xEFC60000

l = 18


untemperRightHelper :: Int -> Int -> Int -> Int -> Int
untemperRightHelper nLeft acc n s
  | nLeft <= s = n `xor` (acc `shiftR` s)
  | otherwise = untemperRightHelper nLeft' acc' n s
    where
        nLeft' = nLeft - s
        acc' = n `xor` (acc `shiftR` s)

untemperRight :: Int -> Int -> Int
untemperRight n s = untemperRightHelper 32 n n s
        
temperRight :: Int -> Int -> Int
temperRight n s = n `xor` (n `shiftR` s)
        

