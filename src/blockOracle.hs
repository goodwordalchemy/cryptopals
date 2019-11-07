module BlockOracle( EncryptionMode(..)
                  , getRandomAESKey
                  , modeOracleWithAnswer
                  , getSimplePaddingOracle
) where

import qualified Data.ByteString as B
import Data.Word(Word8)
import System.Random

import qualified Lib

type BLetterStream = [Word8]
data EncryptionMode = ECB | CBC deriving (Show, Eq)

getRandomLetterStream :: StdGen -> (BLetterStream, StdGen)
getRandomLetterStream g = (randomRs (0, 255) g, g)

randomByteString :: BLetterStream -> Int -> (B.ByteString, BLetterStream)
randomByteString rLetters n = (result, rest)
    where result = B.pack these
          (these, rest) = splitAt n rLetters

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

getRandomAESKey :: StdGen -> (B.ByteString, StdGen)
getRandomAESKey gen = (key, gen')
    where 
        (key, _) = randomByteString letterStream 16
        (letterStream, gen') = getRandomLetterStream gen


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


getSimplePaddingOracle :: B.ByteString -> B.ByteString -> IO B.ByteString
getSimplePaddingOracle key plainText  = do
    gen <- newStdGen
    let aes = Lib.initAES128 key
        (garbledPlainText, _) = getGarbledPlainText gen plainText
        cipherText = Lib.ecbEncryption aes garbledPlainText
    return cipherText
