import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Crypto.Cipher

import Lib

initAES128 :: B.ByteString -> AES128
initAES128 = either (error . show) cipherInit . makeKey

ecbDecryption :: AES128 -> B.ByteString -> B.ByteString
ecbDecryption ctx cipherText = ecbDecrypt ctx cipherText

loadEncryptedFile :: String -> IO B.ByteString
loadEncryptedFile filename = do
    contents <- B.readFile filename
    return $ Lib.base64ToBytes contents

decryptFile :: String -> IO ()
decryptFile filename = do
    cipherText <- loadEncryptedFile filename
    let aes = initAES128 $ BC.pack "YELLOW SUBMARINE" 
        results = ecbDecryption aes cipherText
    print results    

main = do
    decryptFile "data/7.txt"
