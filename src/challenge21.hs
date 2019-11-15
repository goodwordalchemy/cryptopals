import Data.Bits((.&.), xor, shift)
import Data.ByteString as B
import Data.Word(Word8)

import qualified Lib

-- Coefficients
w = 32
n = 624
m = 397
r = 31

a = 0x9908B0DF

u = 11
f = 0xFFFFFFFF

s = 7
b = 0x9D2C5680

t = 15
c = 0xEFC60000

l = 18

lowestWBits :: Int -> Int
lowestWBits x = x .&. (2^w-1)

lowerMask = (1 `shift` r) - 1 
upperMask = lowestWBits (complement lowerMask)

type MTState = (Int, B.ByteString)

seedMtHelper :: Int -> Int -> Int -> B.ByteString
seedMtHelper idx stop prev
  | idx == stop = B.empty
  | otherwise = thisWord `B.cons` (seedMtHelper (idx+1) stop thisValue)
    where
        thisWord = (fromIntegral thisValue)::Word8
        thisValue = lowestWBits (f * (prev `xor` (prev `shift` (-w+2))) + idx)


seedMt :: Int -> MTState
seedMt seed = (n, mtState)
    where 
        mtState = seedMtHelper 0 n seed

twistHelper :: Int -> Int -> B.ByteString -> B.ByteString
twistHelper idx stop prev
  | idx == stop = prev
  | otherwise = twistHelper (idx+1) stop cur
    where 
        cur = Lib.replaceAtIndex idx thisValue prev
        thisValue = ((prev `B.index` (idx + m) `mod` n)) `xor` xA'

        xA' = if (x `mod` 2) /= 0 then xA `xor` a else xA
        xA = x `shift` (-1)

        x = ((prev `B.index` idx) .&. upperMask) 
          + ((prev `B.index` ((idx+1) `mod` n)) .&. lowerMask)

twist :: MTState -> MTState
twist (_, prev) = (0, twistHelper 0 n prev)

