module Challenge15(challenge15) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Either(partitionEithers)

import qualified Lib

validExample :: B.ByteString
validExample = BC.pack "ICE ICE BABY\x04\x04\x04\x04"

failExample1 :: B.ByteString
failExample1 = BC.pack "ICE ICE BABY\x05\x05\x05\x05"


failExample2 :: B.ByteString
failExample2 = BC.pack "ICE ICE BABY\x01\x02\x03\x04"

stripValidPadding :: B.ByteString -> Either String B.ByteString
stripValidPadding text
  | B.length lastChunk /= 16 = Left "block is not 16 bytes long"
  | lastCharVal > 16 = Right text
  | B.all (== lastChar) paddedPart = Right unpaddedPart
  | otherwise = Left ("block is incorrectly padded" ++ show lastChunk)
  where 
      lastChar = B.last text
      lastCharVal = (fromIntegral $ lastChar)::Int
      lastChunk = last $ Lib.chunks16 text
      (unpaddedPart, paddedPart) = B.splitAt (16-lastCharVal) lastChunk


challenge15 :: (String, Int)
challenge15 = (validResult, length lefts)
    where
        (lefts, rights) = partitionEithers
                        $ map stripValidPadding [ validExample
                                                , failExample1
                                                , failExample2 ]
        validResult = BC.unpack $ (rights !! 0)

main :: IO ()
main = do
    print challenge15
