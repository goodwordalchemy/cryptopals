
module Challenge14(challenge14) where

import Data.List
import qualified Data.ByteString as B
import Data.Word(Word8)
import Debug.Trace
import System.Random

import BlockOracle
import qualified Lib

getOracle :: IO Oracle
getOracle = do
    prefix <- getRandomPrefix
    gen <- getStdGen
    let (key, gen') = getRandomAESKey gen
        oracle = getRandomPrefixOracle key prefix
    return oracle

--- TESTS ---

challenge14 :: IO (Bool, [String])
challenge14 = do
    oracle <- getOracle
    let result = take 2 . words . Lib.bytesToString . decryptUnknown $ oracle
        (_, prefixLength) = getPrefix oracle
    return (prefixLength > 0, result)

testDecryptUnknown :: IO ()
testDecryptUnknown = do
    oracle <- getOracle
    let result = decryptUnknown oracle
    putStrLn "================"
    putStrLn "Testing decryption"
    putStr "What is this? =>"
    print result

    
main :: IO ()
main = do
    testDecryptUnknown
