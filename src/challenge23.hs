module Challenge23(challenge23) where

import Data.Bits
import Data.Char(intToDigit)
import Debug.Trace
import Numeric(showIntAtBase)
import Text.Printf


import MersenneTwister(getMtInts, getMtIntsState, mtInt, MTState, seedMt)

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


-- For debugging
temperRight :: Int -> Int -> Int
temperRight n s = n `xor` (n `shiftR` s)

temperLeft :: Int -> Int -> Int -> Int
temperLeft n s m = n `xor` ((n `shift` s) .&. m)

temper :: Int -> Int
temper y = y''''
    where
        y' = y `xor` ((y `shiftR` u) .&. d)
        y'' = y' `xor` ((y' `shift` s) .&. b)
        y''' = y'' `xor` ((y'' `shift` t) .&. c)
        y'''' = y''' `xor` (y''' `shiftR` l)

-- For attacking
untemperRightHelper :: Int -> Int -> Int -> Int -> Int
untemperRightHelper nLeft acc n s
  | nLeft <= s = n `xor` (acc `shiftR` s)
  | otherwise = untemperRightHelper nLeft' acc' n s
    where
        nLeft' = nLeft - s
        acc' = n `xor` (acc `shiftR` s)


untemperRight :: Int -> Int -> Int
untemperRight n s = untemperRightHelper 32 n n s

untemperLeftHelper :: Int -> Int -> Int -> Int -> Int -> Int
untemperLeftHelper nLeft acc n s m
  | nLeft <= s = n `xor` ((acc `shift` s) .&. m)
  | otherwise = untemperLeftHelper nLeft' acc' n s m
    where
        nLeft' = nLeft - s
        acc' = n `xor` ((acc `shift` s) .&. m)

untemperLeft :: Int -> Int -> Int -> Int
untemperLeft n s m = untemperLeftHelper 32 n n s m

untemper :: Int -> Int
untemper y'''' = y
    where
        y''' = untemperRight y'''' l
        y'' = untemperLeft y''' t c
        y' = untemperLeft y'' s b
        y = untemperRight y' u

cloneMtFromNums :: [Int] -> MTState
cloneMtFromNums nums 
  | length nums /= n = error "need 624 nums to clone mt state"
  | otherwise = (n, map untemper nums)

cloneMt :: MTState -> MTState
cloneMt mt = cloneMtFromNums nums
    where nums = getMtInts 624 mt

-- Tests
getTestOutputs :: ([Int], [Int])
getTestOutputs = 
    let mt = seedMt 42
        (firstN, mt') = getMtIntsState n mt
        clone = cloneMt mt
        first10 = getMtInts 10 mt'
        first10' = getMtInts 10 clone
    in (first10, first10')

challenge23 :: Bool
challenge23 = expected == actual
    where (expected, actual) = getTestOutputs

main :: IO ()
main = do
    let (first10, first10') = getTestOutputs
    print $ "orig:" ++ show first10
    print $ "clone:" ++ show first10'
