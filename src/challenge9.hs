module Challenge9(challenge9) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(ord)
import Data.Word(Word8)

import qualified Lib

challenge9 :: [Int]
challenge9 = [result1, result2, result3]
    where
        bytesToPad = Lib.stringToBytes "YELLOW SUBMARINE"
        twentyPad = Lib.padToLength bytesToPad 20
        result1 = B.length $ twentyPad
        result2 = B.length $ Lib.padToMultiple bytesToPad 15
        result3 = ord $ BC.last twentyPad


main :: IO ()
main = do
    print $ Lib.stringToBytes "should be 'YELLOW SUBMARINE\x04\x04\x04\x04' ==>"
    -- print $ B.unpack $ Lib.stringToBytes 
    let bytesToPad = Lib.stringToBytes "YELLOW SUBMARINE"

    print "=========================="

    let result = Lib.padToLength bytesToPad 20
    print result
    print "this should be 20 ==>"
    print $ B.length result 

    print "=========================="
    print "this should be 30 ==>"
    print $ B.length $ Lib.padToMultiple bytesToPad 15
