module Challenge2 (challenge2, expectedCipherText) where 

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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

challenge2 :: B.ByteString
challenge2 = xorHexes plainText encryptionKey
