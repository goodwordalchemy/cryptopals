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
-- unknown = Lib.stringToBytes "12345678900987654321"
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

chunks16 :: B.ByteString -> [B.ByteString]
chunks16 = Lib.splitIntoChunks 16

byteThatMakesMatchingPayload :: Oracle -> B.ByteString -> B.ByteString -> Int -> Word8 -> Word8
byteThatMakesMatchingPayload oracle payload soFar blockNum guessByte = (trace $ "bTMMP==> " ++ show guessByte ++ ", payload:" ++ show payload ++ ", guess:" ++ show (payload `B.snoc` guessByte)) $
    if guess == toMatch
        then guessByte
        else if guessByte == 255 
            then error $ "didn't find a matching byte for payloadChunk:" ++ show toMatch
            else byteThatMakesMatchingPayload oracle payload soFar blockNum nextByte

    where
        nextByte = guessByte + 1
        toMatchOracleOut = oracle payload
        toMatch = blockICareAbout toMatchOracleOut
        guessOracleOut = oracle $ payload `B.append` soFar `B.snoc` guessByte
        guess = blockICareAbout guessOracleOut

        blockICareAbout :: B.ByteString -> B.ByteString
        blockICareAbout oracleOut = (chunks16 oracleOut) !! blockNum
          


decryptNextByte :: Oracle -> B.ByteString -> Word8
decryptNextByte oracle soFar = (trace $ "payload:" ++ show payload ++  ", soFar:" ++ show soFar ++ ", byte:" ++ show byte ++ ", padLength:" ++ show padLength) $
    byte
    where 
        soFarLength = B.length soFar
        blockNum = soFarLength `div` 16
        padLength = 15 - (soFarLength `mod` 16)
        payload = nBytePayload padLength 
        byte = byteThatMakesMatchingPayload oracle payload soFar blockNum 0
        

byteAtATimeHelper :: Oracle -> B.ByteString -> Int-> B.ByteString
byteAtATimeHelper oracle decryptedSoFar stopLength = (trace $ "BAATHelper==> decrypted:" ++ show decrypted ++ ", stopLength:" ++ show stopLength) $
    if B.length decrypted >= stopLength 
       then (trace "BAATHelper==>done") $ decrypted
       else (trace "BAATHelper==>doing recursive call...") $ byteAtATimeHelper oracle decrypted stopLength
    where
        decrypted = decryptedSoFar `B.snoc` nextByte
        nextByte = decryptNextByte oracle decryptedSoFar


byteAtATime :: Oracle -> B.ByteString
byteAtATime oracle = byteAtATimeHelper oracle B.empty lengthOfUnknown
    where 
        lengthOfUnknown = B.length unknown
        -- lengthOfUnknown = B.length . oracle $ nBytePayload 0

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
