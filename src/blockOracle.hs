module BlockOracle( byteAtATime
                  , decryptUnknown
                  , detectBlockSize
                  , EncryptionMode(..)
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

byteThatMakesMatchingPayload :: Oracle -> B.ByteString -> B.ByteString -> Int -> Word8 -> Word8
byteThatMakesMatchingPayload oracle payload soFar blockNum guessByte = 
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
        blockICareAbout oracleOut = (Lib.chunks16 oracleOut) !! blockNum

decryptNextByte :: Oracle -> B.ByteString -> Word8
decryptNextByte oracle soFar = byte
    where 
        soFarLength = B.length soFar
        blockNum = soFarLength `div` 16
        padLength = 15 - (soFarLength `mod` 16)
        payload = nBytePayload padLength 
        byte = byteThatMakesMatchingPayload oracle payload soFar blockNum 0
        

byteAtATimeHelper :: Oracle -> B.ByteString -> Int-> B.ByteString
byteAtATimeHelper oracle decryptedSoFar stopLength =
    if B.length decrypted >= stopLength 
       then decrypted
       else byteAtATimeHelper oracle decrypted stopLength
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
