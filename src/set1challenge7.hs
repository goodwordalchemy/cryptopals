import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Lib


loadEncryptedFile :: String -> IO B.ByteString
loadEncryptedFile filename = do
    contents <- B.readFile filename
    return $ Lib.base64ToBytes contents

decryptFile :: String -> IO ()
decryptFile filename = do
    cipherText <- loadEncryptedFile filename
    let aes = Lib.initAES128 $ BC.pack "YELLOW SUBMARINE" 
        results = Lib.ecbDecryption aes cipherText
    print results    

main = do
    decryptFile "data/7.txt"
