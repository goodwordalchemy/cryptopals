import Data.Bits((.&.), xor, shift)
import Data.ByteString as B
import Data.Word(Word8)

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

seedMtHelper :: Int -> Int -> Int -> B.ByteString
seedMtHelper idx stop prev
  | idx == stop = B.empty
  | otherwise = thisWord `B.cons` (seedMtHelper (idx+1) stop thisValue)
    where
        thisWord = (fromIntegral thisValue)::Word8
        thisValue = lowestWBits (f * (prev `xor` (prev `shift` (w-2))) + idx)
        lowestWBits x = x .&. (2^w-1)


seedMt :: Int -> (Int, B.ByteString)
seedMt seed = (n, mtState)
    where 
        mtState = seedMtHelper 0 n seed
