module BlockOracle( byteAtATime
                  , decryptUnknown
                  , detectBlockSize
                  , EncryptionMode(..)
                  , getPrefix
                  , getSimpleOracle
                  , getRandomAESKey
                  , getRandomPrefix
                  , getRandomPrefixOracle
                  , guessEncryptionMode
                  , modeOracleWithAnswer
                  , nBytePayload
                  , Oracle
                  , unknown
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word8)
import Debug.Trace
import System.Random

import qualified Lib
import Lib(getRandomAESKey, getRandomLetterStream, randomByteString)

data EncryptionMode = ECB | CBC deriving (Show, Eq)
type Oracle = B.ByteString -> B.ByteString

unknown :: B.ByteString
unknown = Lib.base64ToBytes . Lib.stringToBytes $ "\
\Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
\aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
\dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
\YnkK"

guessEncryptionMode :: B.ByteString -> EncryptionMode
guessEncryptionMode cipherText = if Lib.detectECB cipherText
                                 then ECB
                                 else CBC

coinFlip :: StdGen -> (Bool, StdGen)
coinFlip g = random g

randomPadLength :: StdGen -> (Int, StdGen)
randomPadLength g = randomR (5,10) g

getGarbledPlainText 
    :: StdGen
    -> B.ByteString 
    -> (B.ByteString, StdGen)
getGarbledPlainText g plainText = (garbled, g'')
    where
        garbled = B.concat [prefix, plainText, suffix]
        (suffix, _) = randomByteString rest suffixLength
        (prefix, rest) = randomByteString letterStream prefixLength

        (letterStream, g''') = getRandomLetterStream g''
        (prefixLength, g'') = randomPadLength g'
        (suffixLength, g') = randomPadLength g


modeOracleWithAnswer :: StdGen -> B.ByteString -> (EncryptionMode, B.ByteString)
modeOracleWithAnswer gen plainText = result
    where
        result = case coinFlipResult of
                   True -> (ECB, Lib.ecbEncryption aes garbledPlainText)
                   False -> (CBC, Lib.cbcEncryption aes iv garbledPlainText)
        aes = Lib.initAES128 key
        (key, gen') = getRandomAESKey gen
        (iv, gen'') = getRandomAESKey gen'
        (garbledPlainText, gen''') = getGarbledPlainText gen'' plainText
        (coinFlipResult, _) = coinFlip gen'''

getSimpleOracle :: B.ByteString -> Oracle
getSimpleOracle key plainText  = cipherText
    where
        aes = Lib.initAES128 key
        fullPlainText = B.append plainText unknown
        cipherText = Lib.ecbEncryption aes fullPlainText

getRandomPrefix :: IO B.ByteString
getRandomPrefix = do
    g <- newStdGen
    let (prefixLength, g') = randomPadLength g
        (letterStream, g'') = getRandomLetterStream g'
        (prefix, _) = randomByteString letterStream prefixLength
    return prefix


getRandomPrefixOracle :: B.ByteString -> B.ByteString -> Oracle
getRandomPrefixOracle key prefix plainText = cipherText
    where
        aes = Lib.initAES128 key
        fullPlainText = B.concat [prefix, plainText, unknown]
        cipherText = Lib.ecbEncryption aes fullPlainText


-- Decryption Tools
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

getPrefixHelper :: Oracle -> Int -> B.ByteString
getPrefixHelper oracle curLength
  | curLength > 50 = error "Prefix is too long and there's probably something wrong"
  | otherwise = if Lib.detectECB oracleOut 
                   then prefix
                   else getPrefixHelper oracle (curLength+1)
   where oracleOut = oracle prefix
         prefix = nBytePayload curLength

getPrefix :: Oracle -> (B.ByteString, Int)
getPrefix oracle = (prefix, nBlocks)
    where 
        prefix = getPrefixHelper oracle 0
        maybeIndex = Lib.findRepetitionIndex (oracle prefix)
        nBlocks = case maybeIndex of
                    Nothing -> error "Couldn't find repetition index"
                    Just idx -> (idx + (B.length prefix)) `div` 16


byteThatMakesMatchingPayload 
    :: Oracle 
    -> B.ByteString 
    -> B.ByteString 
    -> Int 
    -> Int 
    -> Word8 
    -> Word8
byteThatMakesMatchingPayload 
    oracle 
    payload 
    soFar 
    prefixLength
    blockNum 
    guessByte = 
        if guess == toMatch
            then guessByte
            else if guessByte == 255 
                then error $ "didn't find a matching byte for payloadChunk:" ++ show toMatch
                else byteThatMakesMatchingPayload 
                        oracle 
                        payload 
                        soFar 
                        prefixLength
                        blockNum 
                        nextByte

    where
        nextByte = guessByte + 1
        toMatchOracleOut = areaICareAbout $ oracle payload
        toMatch = blockICareAbout toMatchOracleOut
        guessOracleOut = oracle $ payload `B.append` soFar `B.snoc` guessByte
        guess = blockICareAbout $ areaICareAbout guessOracleOut

        blockICareAbout :: B.ByteString -> B.ByteString
        blockICareAbout oracleOut = (Lib.chunks16 oracleOut) !! blockNum

        areaICareAbout :: B.ByteString -> B.ByteString
        areaICareAbout oracleOut = snd $ B.splitAt prefixLength oracleOut

decryptNextByte :: Oracle -> B.ByteString -> B.ByteString -> Int -> Word8
decryptNextByte oracle soFar prefix skipBlocks = byte
    where 
        soFarLength = B.length soFar
        blockNum = (soFarLength `div` 16)
        padLength = 15 - (soFarLength `mod` 16)
        payload = prefix `B.append` nBytePayload padLength 
        prefixLength = B.length prefix
        byte = byteThatMakesMatchingPayload 
                    oracle 
                    payload 
                    soFar 
                    prefixLength 
                    blockNum 
                    0

byteAtATimeHelper 
    :: Oracle 
    -> B.ByteString 
    -> B.ByteString 
    -> Int
    -> Int
    -> B.ByteString
byteAtATimeHelper oracle decryptedSoFar prefix skipBlocks stopLength =
    if B.length decrypted >= stopLength 
       then decrypted
       else byteAtATimeHelper oracle decrypted prefix skipBlocks stopLength
    where
        decrypted = decryptedSoFar `B.snoc` nextByte
        nextByte = decryptNextByte oracle decryptedSoFar prefix skipBlocks

byteAtATime :: Oracle -> B.ByteString
byteAtATime oracle = byteAtATimeHelper 
                        oracle 
                        B.empty 
                        prefix
                        skipBlocks 
                        lengthOfUnknown
    where 
        lengthOfUnknown = B.length unknown
        (prefix, skipBlocks) = getPrefix oracle

decryptUnknown :: Oracle -> B.ByteString
decryptUnknown oracle = case mode of 
        CBC -> error "Can't decrypt cbc mode yet"
        ECB -> byteAtATime oracle
    where mode = guessEncryptionMode . oracle $ nBytePayload (3*blockSize)
          blockSize = detectBlockSize oracle
