module Challenge12() where

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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


-- byteAtAtime :: Oracle -> IO B.ByteString
-- byteAtATime oracle = 

-- decryptUnknown :: Oracle -> IO B.ByteString
-- decryptUnknown oracle = do
--     blockSize <- detectBlockSize oracle
--     let mode = guessEncryptionMode . oracle $ nBytePayload (3*blockSize)
--     if mode == CBC
--         then error "Can't decrypt cbc mode yet"
--         else byteAtATime oracle
--
----

testDetectBlockSize :: IO ()
testDetectBlockSize = do
    gen <- getStdGen
    let (key, _) = getRandomAESKey gen
        oracle = getSimpleOracle key
        blockSize = detectBlockSize oracle
    putStrLn "================"
    putStr "This should be 16 ==>"
    putStrLn $ show blockSize
    
main :: IO ()
main = do
    testDetectBlockSize
