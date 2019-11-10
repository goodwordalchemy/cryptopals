module BlockOracle( EncryptionMode(..)
                  , guessEncryptionMode
                  , getRandomAESKey
                  , modeOracleWithAnswer
                  , Oracle
                  , getSimpleOracle
                  , unknown
) where

import qualified Data.ByteString as B
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


getRandomPrefixOracle :: B.ByteString -> B.ByteString -> B.ByteString
getRandomPrefixOracle key plainText = do
    g <- newStdGen
    let (prefixLength, g') = randomPadLength g
        (letterStream, g'') = getRandomLetterStream g'
        (prefix, _) = randomByteString letterStream prefixLength
        aes = Lib.initAES128 key
        fullPlainText = B.concat [prefix, plainText, unknown]
        cipherText = Lib.ecbEncryption aes fullPlainText
    cipherText
