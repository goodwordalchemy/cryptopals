import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word8)
import Debug.Trace
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
        


encryptionOracleWithAnswer :: StdGen -> B.ByteString -> (EncryptionMode, B.ByteString)
encryptionOracleWithAnswer gen plainText = result
    where
        result = case coinFlipResult of
                   True -> (ECB, Lib.ecbEncryption aes garbledPlainText)
                   False -> (CBC, Lib.cbcEncryption aes iv garbledPlainText)
        aes = Lib.initAES128 key
        (key, gen') = getRandomAESKey gen
        (iv, gen'') = getRandomAESKey gen'
        (garbledPlainText, gen''') = getGarbledPlainText gen'' plainText
        (coinFlipResult, _) = coinFlip gen'''

guessEncryptionMode :: B.ByteString -> EncryptionMode
guessEncryptionMode cipherText = if Lib.detectECB cipherText
                                 then ECB
                                 else CBC

{- 
The goal is to find a length where no matter what padding comes before or
after, there will always be at least two identical 16 byte blocks of text. 

Well, if there are 10 bytes of padding, then the repeated character will be
split as follows: 6 + 16 + 16 + etc.  So we would only need 38 repeated bytes.

BUT, what if there are only 5 bytes of prefix?  The repeated character would
be split as follows: 11 + 16 + 16 + etc.  So we sould need 32 + 11 repeated
bytes.
-}
oracleInput :: B.ByteString
oracleInput = Lib.stringToBytes $ replicate (32+11) 'A'

detectionSuccessesForTrials :: Int -> Int -> IO Int
detectionSuccessesForTrials 0 nSuccesses = return nSuccesses
detectionSuccessesForTrials nTrials nSuccesses = do
    gen <- newStdGen
    let (actualMode, cipherText) = encryptionOracleWithAnswer gen oracleInput
        guessedMode = guessEncryptionMode cipherText
    if guessedMode == actualMode
       then detectionSuccessesForTrials (nTrials-1) (nSuccesses+1)
       else detectionSuccessesForTrials (nTrials-1) (nSuccesses)
    

testDetectEncryptionMode :: IO ()
testDetectEncryptionMode = do
    let nTrials = 100
    putStr $ "# of successful detections in " ++ (show nTrials) ++ " trials ==>"
    nSuccesses <- detectionSuccessesForTrials nTrials 0
    putStrLn $ show nSuccesses
    let percent = (fromIntegral nSuccesses) / (fromIntegral nTrials) * 100
    putStrLn $ "That's " ++ (show percent) ++ "%"
