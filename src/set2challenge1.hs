import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word8)

import qualified Lib

padToLength :: B.ByteString -> Int -> B.ByteString
padToLength text size = B.append text padding
    where padding = B.replicate diff (fromIntegral diff)
          diff = size - B.length text

main :: IO ()
main = do
    print $ Lib.stringToBytes "should be 'YELLOW SUBMARINE\x04\x04\x04\x04' ==>"
    -- print $ B.unpack $ Lib.stringToBytes 
    let bytesToPad = Lib.stringToBytes "YELLOW SUBMARINE"

    print "=========================="

    let result = padToLength bytesToPad 20
    print result
    print "this should be 20 ==>"
    print $ B.length result 
