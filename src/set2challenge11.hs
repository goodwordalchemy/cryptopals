import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word8)
import System.Random

import qualified Lib

type BLetterStream = [Word8]

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
    :: BLetterStream -> B.ByteStream -> (B.ByteStream, BLetterStream)
getGarbledPlainText rLetters plainText = (garbled, rest)
    where
        garbled = B.concat [prefix, plainText, suffix]

encryptionOracleWithAnswer :: B.ByteString -> IO (Bool, B.ByteString)
encryptionOracleWithAnswer plainText = do
    stdGen <- getStdGen
    (key, newGen) <- generateRandomAESKey stdGen
    (iv, newGen') <- generateRandomAESKey newGen
    garbledPlainText <- garblePlainText stdGen plainText
    coinFlipResult <- coinFlip stdGen
    
    let aes = Lib.initAES128 key
        cipherText = if coinFlipResult 
                     then Lib.ecbEncryption aes garbledPlainText
                     else Lib.cbcEncryption aes iv garbledPlainText
    
    return (coinFlipResult, cipherText)

encryptionOracle :: B.ByteString -> IO B.ByteString
encryptionOracle plainText = snd <$> encryptionOracleWithAnswer
