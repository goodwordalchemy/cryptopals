module Challenge12() where

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random

import BlockOracle
import qualified Lib

type Oracle = B.ByteString -> IO B.ByteString

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

unkown :: B.ByteString
unkown = Lib.base64ToBytes . Lib.stringToBytes $ "\
\Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
\aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
\dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
\YnkK"

appendCipherText :: B.ByteString -> B.ByteString
appendCipherText knownPlainText = B.append knownPlainText unknown

nBytePlainText :: Int -> B.ByteString
nBytePlainText n = BC.replicate n 'A'

nBytePayload :: Int -> B.ByteString
nBytePayload n = appendCipherText (nBytePlainText n)

getNOracleOutputsHelper
    :: Oracle 
    -> B.ByteString 
    -> Int 
    -> [B.ByteString] 
    -> IO [B.ByteString]
getNOracleOutputsHelper oracle plainText nTrials acc
    | nTrials == 0 = return acc
    | otherwise = do
        oracleOut <- oracle plainText
        result <- getNOracleOutputsHelper 
                    oracle plainText (nTrials-1) (oracleOut:acc)
        return result


getNOracleOutputs
    :: Oracle 
    -> B.ByteString 
    -> Int 
    -> IO [B.ByteString]
getNOracleOutputs oracle plainText nTrials = result
    where result = getNOracleOutputsHelper oracle plainText nTrials []

mostCommonLength :: Oracle -> Int -> IO Int
mostCommonLength oracle plainTextSize = do
    let plainText = nBytePayload plainTextSize
        nTrials = 20
    results <- getNOracleOutputs oracle plainText nTrials
    return $ mostCommon . map B.length $ results

detectBlockSizeHelper :: Oracle -> Int -> Int -> IO Int
detectBlockSizeHelper oracle thisSize prevCipherSize = do
    thisCipherSize <- mostCommonLength oracle thisSize
    if thisCipherSize == prevCipherSize
       then detectBlockSizeHelper oracle (thisSize+1) thisCipherSize
       else return $ thisCipherSize - prevCipherSize

detectBlockSize :: Oracle -> IO Int
detectBlockSize oracle = do
    initialCipherSize <- mostCommonLength oracle 1
    blockSize <- detectBlockSizeHelper oracle 2 initialCipherSize
    return blockSize

byteAtAtime :: Oracle -> IO B.ByteString
byteAtATime oracle = 

decryptUnknown :: Oracle -> IO B.ByteString
decryptUnknown oracle = do
    blockSize <- detectBlockSize oracle
    let mode = guessEncryptionMode . oracle $ nBytePayload (3*blockSize)
    if mode == CBC
        then error "Can't decrypt cbc mode yet"
        else byteAtATime oracle

----

testDetectBlockSize :: IO ()
testDetectBlockSize = do
    gen <- getStdGen
    let (key, _) = getRandomAESKey gen
        oracle = getSimplePaddingOracle key
    blockSize <- detectBlockSize oracle
    putStrLn "================"
    putStr "This should be 16 ==>"
    putStrLn $ show blockSize
    
main :: IO ()
main = do
    testDetectBlockSize
