module Challenge11(challenge11) where

import qualified Data.ByteString as B
import System.Random

import BlockOracle
import qualified Lib


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
    let (actualMode, cipherText) = modeOracleWithAnswer gen oracleInput
        guessedMode = guessEncryptionMode cipherText
    if guessedMode == actualMode
       then detectionSuccessesForTrials (nTrials-1) (nSuccesses+1)
       else detectionSuccessesForTrials (nTrials-1) (nSuccesses)
    

challenge11 :: IO Int
challenge11 = do
    let nTrials = 100
    -- putStr $ "# of successful detections in " ++ (show nTrials) ++ " trials ==>"
    nSuccesses <- detectionSuccessesForTrials nTrials 0
    -- putStrLn $ show nSuccesses
    let percent = (fromIntegral nSuccesses) / (fromIntegral nTrials) * 100
    -- putStrLn $ "That's " ++ (show percent) ++ "%"
    return $ round percent
