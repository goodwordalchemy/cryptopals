module Challenge12() where

import qualified Data.ByteString as B

import BlockOracle
import qualified Lib

type Oracle = B.ByteString -> IO B.ByteString

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

cipherText :: B.ByteString
cipherText = Lib.base64ToBytes . Lib.stringToBytes $ "\
\Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
\aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
\dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
\YnkK"

appendCipherText :: B.ByteString -> B.ByteString
appendCipherText knownPlainText = B.append knownPlainText cipherText

nBytePlainText :: Int -> B.ByteString
nBytePlainText n = B.replicate n 'A'

nBytePayload :: Int -> B.ByteString
nBytePayload n = appendCipherText (nBytePlainText n)

getNOracleOutputsHelper
    :: Oracle 
    -> B.ByteString 
    -> Int 
    -> [B.ByteString] 
    -> IO [B.ByteString]
getNOracleOutputsHelper oracle plainText nTrials acc
    | nTrials == 0 = return acc
    | otherwise = do
        return getNOracleOutputs oracle (nTrials-1) plainText (result:acc)
    where
        result = oracle plainText

getNOracleOutputs
    :: Oracle 
    -> B.ByteString 
    -> Int 
    -> IO [B.ByteString]
getNOracleOutputs oracle plainText nTrials = result
    where result = getNOracleOutputs oracle plainText nTrials []

mostCommonCipherTextLength :: Oracle -> B.ByteString -> IO Int
mostCommonCipherTextLength oracle plainText = do
    outputs <- getNOracleOutputs oracle plainText 20
    return $ mostCommon results

-- detectBlockSize :: Oracle -> Int
-- detectBlockSize oracle = blockSize
--     where
       
