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
unknown = Lib.stringToBytes "XYZ"
-- unknown = Lib.base64ToBytes . Lib.stringToBytes $ "\
-- \Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
-- \aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
-- \dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
-- \YnkK"

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

-- findMatchCharHelper :: Oracle -> B.ByteString -> Word8 -> Word8 -> Word8
-- findMatchCharHelper oracle pad charToMatch curChar = 
--     (trace $ "findMatchCharHelper==> charToMatch: " ++ show charToMatch ++ ", actualChar: " ++ show (oracleOut `B.index` idxToCheck) ++", trying char:" ++ show curChar) $
--     if isMatch
--         then curChar
--         else if curChar < 255
--             then findMatchCharHelper oracle pad charToMatch nextChar
--             else error $ "Could not find matching character for " ++ show curChar
--     where
--         nextChar = curChar + 1
--         isMatch = oracleOut `B.index` idxToCheck == charToMatch
--         idxToCheck = B.length pad
--         oracleOut = oracle paddedChar
--         paddedChar = pad `B.snoc` curChar
--
-- findMatchChar :: Oracle -> B.ByteString -> Word8 -> Word8
-- findMatchChar oracle pad charToMatch = matchChar
--     where matchChar = findMatchCharHelper oracle pad charToMatch 0
--
-- decryptByteAtN :: Oracle -> Int -> Word8
-- decryptByteAtN oracle n = (trace $ "decryptByteAtN==> padLength:" ++ show padLength ++ ", charToMatch: " ++ show charToMatch)
--                         $ findMatchChar oracle pad charToMatch
--     where
--         pad = nBytePayload padLength
--         padLength = 15 - (n `mod` 16) + (n `div` 16 * 16)
--         charToMatch = (oracle pad) `B.index` padLength
--
-- byteAtATime :: Oracle -> B.ByteString
-- byteAtATime oracle = (trace $ "Length of unknown: " ++ show lengthOfUnknown) 
--                    $ B.pack $ map decryptByteAtN' [0..lengthOfUnknown-1]
--     where 
--         lengthOfUnknown = B.length . oracle $ nBytePayload 0
--         decryptByteAtN' = decryptByteAtN oracle

chunks16 :: B.ByteString -> [B.ByteString]
chunks16 = Lib.splitIntoChunks 16

byteThatMakesMatchingPayload :: Oracle -> B.ByteString -> Int -> Word8 -> Word8
byteThatMakesMatchingPayload oracle payload blockNum guessByte =
    if guess == toMatch
        then guessByte
        else byteThatMakesMatchingPayload oracle payload blockNum nextByte

    where
        nextByte = guessByte + 1
        toMatchOracleOut = oracle payload
        toMatch = blockICareAbout toMatchOracleOut
        guessOracleOut = oracle $ payload `B.snoc` guessByte
        guess = blockICareAbout guessOracleOut

        blockICareAbout :: B.ByteString -> B.ByteString
        blockICareAbout oracleOut = (chunks16 oracleOut) !! blockNum
          


decryptNextByte :: Oracle -> B.ByteString -> Word8
decryptNextByte oracle soFar = (trace $ "payload:" ++ show payload ++  ", soFar:" ++ show soFar ++ ", byte:" ++ show byte ++ ", padLength:" ++ show padLength) $
    byte
    where 
        soFarLength = B.length soFar
        blockNum = soFarLength `div` 16
        padLength = 15 - (soFarLength `mod` 16) + (blockNum * 16)
        payload = (nBytePayload padLength) `B.append` soFar
        byte = byteThatMakesMatchingPayload oracle payload blockNum 0
        

byteAtATimeHelper :: Oracle -> B.ByteString -> Int-> B.ByteString
byteAtATimeHelper oracle decryptedSoFar stopLength = 
    if B.length decrypted == stopLength 
       then decrypted
       else byteAtATimeHelper oracle decrypted stopLength
    where
        decrypted = decryptedSoFar `B.snoc` nextByte
        nextByte = decryptNextByte oracle decryptedSoFar


byteAtATime :: Oracle -> B.ByteString
byteAtATime oracle = byteAtATimeHelper oracle B.empty lengthOfUnknown
    where 
        lengthOfUnknown = B.length . oracle $ nBytePayload 0

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
