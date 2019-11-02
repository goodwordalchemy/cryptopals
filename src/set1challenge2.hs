module Challenge_2 () where 

import qualified Data.ByteString as B
import Lib

xorHexes :: B.ByteString -> B.ByteString -> B.ByteString
xorHexes p ek = Lib.bytesToHex
              $ Lib.fixedXOR (Lib.hexToBytes ek) (Lib.hexToBytes p)

plainText :: B.ByteString
plainText = Lib.stringToBytes "1c0111001f010100061a024b53535009181c"

encryptionKey :: B.ByteString
encryptionKey = Lib.stringToBytes "686974207468652062756c6c277320657965"

expectedCipherText :: B.ByteString
expectedCipherText = Lib.stringToBytes "746865206b696420646f6e277420706c6179"

challenge_2 :: String
challenge_2 = Lib.bytesToString $ xorHexes plainText encryptionKey

main :: IO ()
main = do
    putStrLn "The should be equal..."
    putStr "expect: "
    B.putStrLn expectedCipherText

    putStr "result: "
    putStrLn $ challenge_2