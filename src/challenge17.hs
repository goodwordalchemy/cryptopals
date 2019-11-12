import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random(mkStdGen)

import qualified Lib

data CipherDirection = Encryption | Decryption
type Device = CipherDirection -> B.ByteString -> B.ByteString

targets :: [B.ByteString]
targets = map (Lib.base64ToBytes . BC.pack) raws
    where 
        raws = [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
               , "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
               , "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
               , "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
               , "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
               , "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
               , "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
               , "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
               , "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
               , "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
               ]

getConstant16ByteString :: Int -> B.ByteString
getConstant16ByteString seed = key 
    where 
        gen = mkStdGen seed
        (key, _) = Lib.getRandomAESKey gen



iv :: B.ByteString
iv = getConstant16ByteString 2

aesKey :: B.ByteString
aesKey = getConstant16ByteString 1

getCBCEncryptionDevice :: Device
getCBCEncryptionDevice = device
    where 
        aes = Lib.initAES128 aesKey

        device direction input = case direction of 
            Encryption -> Lib.cbcEncryption aes iv input
            Decryption -> Lib.cbcDecryption aes iv input

{- Note, in final implementation of this, a random number will be passed
 to this function, and I will have to maintain a mapping of ciphertexts
 to decryption states
-}

padAndEncrypt :: B.ByteString -> (B.ByteString, B.ByteString)
padAndEncrypt target = (iv, encrypted)
    where
        encrypted = getCBCEncryptionDevice Encryption paddedTarget
        paddedTarget = Lib.padToMultiple target 16

encryptedTarget :: (B.ByteString, B.ByteString)
encryptedTarget = padAndEncrypt target
    where
        target = targets !! 0

paddingIsValid :: B.ByteString -> Bool
paddingIsValid  encrypted = case Lib.stripValidPadding decrypted of
        Right x -> True
        Left y -> False
    where
        decrypted = getCBCEncryptionDevice Decryption encrypted

testEncryptionAndDecryption :: IO ()
testEncryptionAndDecryption = do
    let target = BC.pack "test123"
        (_, encrypted) = padAndEncrypt target
        result = paddingIsValid encrypted

        encrypted' = 

    print $ result1


main :: IO ()
main = do
    testEncryptionAndDecryption
