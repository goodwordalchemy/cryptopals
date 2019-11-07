module Challenge12() where

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word8)
import Debug.Trace
import System.Random

import BlockOracle
import qualified Lib

type Oracle = B.ByteString -> B.ByteString

unknown :: B.ByteString
unknown = Lib.base64ToBytes . Lib.stringToBytes $ "\
\Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
\aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
\dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
\YnkK"

getSimpleOracle :: B.ByteString -> Oracle
getSimpleOracle key plainText  = cipherText
    where
        aes = Lib.initAES128 key
        fullPlainText = B.append plainText unknown
        cipherText = Lib.ecbEncryption aes fullPlainText

appendCipherText :: B.ByteString -> B.ByteString
appendCipherText knownPlainText = B.append knownPlainText unknown

nBytePayload :: Int -> B.ByteString
nBytePayload n = BC.replicate n 'A'

detectBlockSizeHelper :: Oracle -> Int -> Int -> Int
detectBlockSizeHelper oracle thisSize prevCipherSize = 
    if thisCipherSize == prevCipherSize
        then detectBlockSizeHelper oracle (thisSize+1) thisCipherSize
        else result
    where 
        result = thisCipherSize - prevCipherSize
        thisCipherSize = B.length thisCipher
        thisCipher = oracle $ nBytePayload thisSize

detectBlockSize :: Oracle -> Int
detectBlockSize oracle = detectBlockSizeHelper oracle 2 initialCipherSize
    where initialCipherSize = B.length . oracle $ nBytePayload 1

findMatchCharHelper :: Oracle -> B.ByteString -> Word8 -> Word8 -> Word8
findMatchCharHelper oracle pad charToMatch curChar = 
    if isMatch
        then curChar
        else if curChar < 255
            then findMatchCharHelper oracle pad charToMatch nextChar
            else error $ "Could not find matching character for " ++ show curChar
    where
        nextChar = traceShowId $ curChar + 1
        isMatch = oracleOut `B.index` idxToCheck == charToMatch
        idxToCheck = B.length pad
        oracleOut = oracle paddedChar
        paddedChar = pad `B.snoc` curChar

findMatchChar :: Oracle -> B.ByteString -> Word8 -> Word8
findMatchChar oracle pad charToMatch = matchChar
    where matchChar = findMatchCharHelper oracle pad charToMatch 0

decryptByteAtN :: Oracle -> Int -> Word8
decryptByteAtN oracle n = findMatchChar oracle pad charToMatch
    where
        pad = nBytePayload padLength
        padLength = 15 - (n `mod` 16) + (n `div` 16 * 16)
        charToMatch = (oracle pad) `B.index` (padLength + 1)


byteAtATime :: Oracle -> B.ByteString
byteAtATime oracle = B.pack $ map decryptByteAtN' [0..lengthOfUnknown-1]
    where 
        lengthOfUnknown = B.length . oracle $ nBytePayload 0
        decryptByteAtN' = decryptByteAtN oracle

decryptUnknown :: Oracle -> B.ByteString
decryptUnknown oracle = case mode of 
        CBC -> error "Can't decrypt cbc mode yet"
        ECB -> byteAtATime oracle
    where mode = guessEncryptionMode . oracle $ nBytePayload (3*blockSize)
          blockSize = detectBlockSize oracle

--- TESTS ---

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
