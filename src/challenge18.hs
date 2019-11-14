module Challenge18(challenge18) where

import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Debug.Trace

import qualified Lib

toDecrypt :: B.ByteString
toDecrypt = Lib.base64ToBytes . BC.pack $ "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="

key :: B.ByteString
key = BC.pack "YELLOW SUBMARINE"

challenge18 :: (Bool, Bool)
challenge18 = (lastLetterCorrect, encryptionCorrect)
    where 
        encryptionCorrect = encrypted == toDecrypt
        encrypted = ctr decrypted

        lastLetterCorrect = lastLetter == ' '
        lastLetter = BC.last decrypted

        decrypted = ctr toDecrypt
        ctr = Lib.getCTRDevice key 0

main :: IO ()
main = do
    print challenge18
