module Challenge7(challenge7) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Lib


loadEncryptedFile :: String -> IO B.ByteString
loadEncryptedFile filename = do
    contents <- B.readFile filename
    return $ Lib.base64ToBytes contents

decryptFile :: String -> IO B.ByteString
decryptFile filename = do
    cipherText <- loadEncryptedFile filename
    let aes = Lib.initAES128 $ BC.pack "YELLOW SUBMARINE" 
        results = Lib.ecbDecryption aes cipherText
    return results

challenge7 :: IO [String]
challenge7 = do
    result <- decryptFile "data/7.txt"
    return $ take 2 . words $ Lib.bytesToString result

main = do
    print <$> decryptFile "data/7.txt"
