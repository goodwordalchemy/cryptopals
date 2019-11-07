module Challenge12() where

import qualified Data.ByteString as B

import BlockOracle
import qualified Lib

cipherText :: B.ByteString
cipherText = Lib.base64ToBytes . Lib.stringToBytes $ "\
\Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
\aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
\dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
\YnkK"

constructPlainText :: B.ByteString -> B.ByteString
constructPlainText knownPlainText = B.append knownPlainText cipherText

type Oracle = B.ByteString -> IO B.ByteString

paddingOracle = getSimplePaddingOracle

getNOracleOutputs 
    :: Oracle 
    -> B.ByteString 
    -> Int 
    -> [B.ByteString] 
    -> IO [B.ByteString]
getNOracleOutputs oracle plainText nTrials acc
    | nTrials == 0 = return acc
    | otherwise = do
        return getNOracleOutputs oracle 
    

detectBlockSize :: Oracle -> Int
detectBlockSize oracle = 

