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

challenge15 :: (String, Int)
challenge15 = (validResult, length lefts)
    where
        (lefts, rights) = partitionEithers
                        $ map Lib.stripValidPadding [ validExample
                                                    , failExample1
                                                    , failExample2 ]
        validResult = BC.unpack $ (rights !! 0)

failExample' :: B.ByteString 
failExample' = BC.pack "1234567890987654"

validExample' :: B.ByteString
validExample' = (BC.pack "1234567890987654") `B.append` (B.replicate 16 16)

testFullPadding :: IO ()
testFullPadding = do
    print $ "# lefts should be 1:" ++ (show $ length lefts)
    print $ "# rights should be 1:" ++ (show $ length rights)
    
    where
        (lefts, rights) = partitionEithers
                        $ map Lib.stripValidPadding [ validExample'
                                                    , failExample' 
                                                    ]

main :: IO ()
main = do
    print challenge15
    testFullPadding
