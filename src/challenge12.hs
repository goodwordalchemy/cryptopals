module Challenge12(challenge12) where

import Data.List
import qualified Data.ByteString as B
import Data.Word(Word8)
import Debug.Trace
import System.Random

import BlockOracle
import qualified Lib

--- TESTS ---

challenge12 :: IO [String]
challenge12 = do
    oracle <- getOracle
    let result = decryptUnknown oracle
    return $ take 2 $ words $ Lib.bytesToString result

getOracle :: IO Oracle
getOracle = do
    gen <- getStdGen
    let (key, _) = getRandomAESKey gen
        oracle = getSimpleOracle key
    return oracle

testDetectBlockSize :: IO ()
testDetectBlockSize = do
    gen <- getStdGen
    let (key, _) = getRandomAESKey gen
        oracle = getSimpleOracle key
        blockSize = detectBlockSize oracle
    putStrLn "================"
    putStrLn "Testing block size"
    putStr "This should be 16 ==>"
    putStrLn $ show blockSize


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
    testDetectBlockSize
    testDecryptUnknown
