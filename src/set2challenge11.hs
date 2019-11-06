import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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

oracleInput :: B.ByteString
oracleInput = Lib.stringToBytes "Fake oracle input"

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
    let nTrials = 10
    putStr $ "# of successful detections in " ++ (show nTrials) ++ " trials ==>"
    nSuccesses <- detectionSuccessesForTrials nTrials 0
    putStrLn $ show nSuccesses
    let percent = (fromIntegral nSuccesses) / (fromIntegral nTrials) * 100
    putStrLn $ "That's " ++ (show percent) ++ "%"
