import Crypto.Cipher(AES128)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random(newStdGen)

import qualified Lib

data CipherDirection = Encryption | Decryption
type Device = CipherDirection -> B.ByteString -> B.ByteString

getCBCEncryptionDevice :: IO Device
getCBCEncryptionDevice = do
    gen <- newStdGen
    let (key, gen') = Lib.getRandomAESKey gen
        (iv, _) = Lib.getRandomAESKey gen
        aes = Lib.initAES128 key
        func = (\direction input -> 
            case direction of 
              Encryption -> Lib.cbcEncryption aes iv input
              Decryption -> Lib.cbcDecryption aes iv input)
    return func

prefix :: B.ByteString
prefix = BC.pack "comment1=cooking%20MCs;userdata="

suffix :: B.ByteString
suffix = BC.pack ";comment2=%20like%20a%20pound%20of%20bacon"

concattedUserInput :: String -> B.ByteString
concattedUserInput userIn = fullIn
    where
        cleanFunc = (\c -> if c `elem` ";=" then '?' else c) 
        cleanIn = BC.map cleanFunc (BC.pack userIn)
        fullIn = B.concat [prefix, cleanIn, suffix]

encryptedUserInput :: Device -> String  -> B.ByteString
encryptedUserInput device userIn = encryptedIn
    where
        fullIn = concattedUserInput userIn
        encryptedIn = device Encryption fullIn


testEncryptedUserInput :: IO ()
testEncryptedUserInput = do
    print $ concattedUserInput "foo=barls;kkd"

    device <- getCBCEncryptionDevice
    print $ encryptedUserInput device "foo=barls;kkd"

main :: IO ()
main = do
    testEncryptedUserInput
