-- module Challenge27(challenge27) where

import Crypto.Cipher(AES128)
import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(chr, ord)
import Data.Word(Word8)
import Debug.Trace
import System.Random(newStdGen)

import qualified Lib

nthBlock16 :: Int -> B.ByteString -> B.ByteString
nthBlock16 n text = (Lib.chunks16 text) !! n

-- Oracle Setup --
data CipherDirection = Encryption | Decryption
type Device = CipherDirection -> B.ByteString -> B.ByteString

getCBCDevice :: B.ByteString -> Device
getCBCDevice key =
    let iv = key
        aes = Lib.initAES128 key
        func = (\direction input -> 
            case direction of 
              Encryption -> Lib.cbcEncryption aes iv input
              Decryption -> Lib.cbcDecryption aes iv input)
    in func

type Oracle = B.ByteString -> Either B.ByteString Bool

getDecryptOracle :: Device -> Oracle
getDecryptOracle device ciphertext 
  | isValid plaintext = Right True
  | otherwise = Left plaintext
    where 
        plaintext = device Decryption ciphertext

        isValid :: B.ByteString -> Bool
        isValid = not . B.any (>= 127)

getEncryptionFunc :: Device -> B.ByteString -> B.ByteString
getEncryptionFunc device = device Decryption

-- Attack
recoverKey :: (B.ByteString -> B.ByteString) -> Oracle -> B.ByteString
recoverKey encryption oracle = key
    where
        key = Lib.fixedXOR p0' p2'
        [p0', _, p2'] = Lib.chunks16 plaintext'

        Left plaintext' = oracle ciphertext'
        
        ciphertext' = B.concat [c0, nulls, c2]
        nulls = B.replicate 16 0

        [c0, _, c2] = Lib.chunks16 ciphertext
        ciphertext = encryption plaintext
        plaintext = BC.replicate (3*16) 'A'


-- Testing
testOracle :: IO ()
testOracle = do
    gen <- newStdGen
    let (key, _) = Lib.getRandomAESKey gen
        device = (trace $ show key) getCBCDevice key
        oracle = getDecryptOracle device
    print $ "should be left => " ++ (show $ oracle (BC.pack "1234567890123456"))
    print $ "should be right => " ++ (show $ oracle $ device Encryption (BC.pack "hello"))

testRecoverKey :: Bool
testRecoverKey =
    let key = BC.pack "YELLOW SUBMARINE"
        device = getCBCDevice key
        oracle = getDecryptOracle device
        encryption = getEncryptionFunc device
        recoveredKey = recoverKey encryption oracle
    in key == recoveredKey

main :: IO ()
main = do
    -- testOracle
    print testRecoverKey
