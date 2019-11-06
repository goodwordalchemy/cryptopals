import Crypto.Cipher(AES128)
import qualified Data.ByteString.Char8 as BC

import qualified Lib


testEcbEncrypt :: IO ()
testEcbEncrypt = do
    let aes = Lib.initAES128 $ Lib.stringToBytes "YELLOW SUBMARINE" 
        message = Lib.stringToBytes "This is my terrific message!1234"
        encrypted = Lib.ecbEncryption aes message
        decrypted = Lib.ecbDecryption aes encrypted
    
    putStrLn "============================"
    putStrLn "TESTING: EcbEncrypt"
    putStrLn "These two strings should be equal:"
    putStr "Plain Text: ==>"
    BC.putStrLn message
    putStr "Decrypted: ==>"
    BC.putStrLn decrypted

testCbc :: IO ()
testCbc = do
    let aes = Lib.initAES128 $ Lib.stringToBytes "YELLOW SUBMARINE"
        iv = BC.pack $ replicate 16 '\x00'
        message = Lib.stringToBytes "AAAAAAAAAAAAAAAAABBBBBBBBBBBBBBB"
        encrypted = Lib.cbcEncryption aes iv message
        decrypted = Lib.cbcDecryption aes iv encrypted
    putStrLn "============================"
    putStrLn "TESTING: Cbc mode functions"
    putStrLn "These two strings should be equal:"
    putStr "Plain Text: ==>"
    BC.putStrLn message
    putStr "Decrypted: ==>"
    BC.putStrLn decrypted
    putStr "Just for fun, here's the ciphertext ==>"
    BC.putStrLn encrypted

filename :: String
filename = "data/10.txt"

loadEncryptedFile :: IO BC.ByteString
loadEncryptedFile = do
    contents <- BC.readFile filename
    return $ Lib.base64ToBytes contents

testLargeFile :: IO ()
testLargeFile = do
    putStrLn "============================"
    putStrLn "TESTING: decrypting a large file"

    cipherText <- loadEncryptedFile
    let aes = Lib.initAES128 $ Lib.stringToBytes "YELLOW SUBMARINE"
        iv = BC.pack $ replicate 16 '\x00'
        plainText = Lib.cbcDecryption aes iv cipherText
    
    BC.putStrLn plainText
    

main :: IO ()
main = do
    testEcbEncrypt
    testCbc
    testLargeFile
