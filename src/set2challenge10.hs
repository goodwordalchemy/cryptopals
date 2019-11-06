import Crypto.Cipher(AES128)
import qualified Data.ByteString.Char8 as BC

import qualified Lib

-- 2. to cbcEncryption,  
--      break the plaintext into 16 byte chunks
--      have an initialization vector
--      xor IV with first chunk of plaintext.  
--      ecb encrypt it
--      The result from the previous step is iv for the next one.  repeat with next block untill the end.
-- 3. test encryption by encrypting and then decrypting something
-- 4. test decryption by decrypting the file.


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
    

main :: IO ()
main = do
    testEcbEncrypt
    testCbc
