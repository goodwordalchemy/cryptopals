module Lib (
    base64ToBytes,
    bytesToBase64,
    hexToBytes,
    bytesToHex,
    bytesToString,
    stringToBytes,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Base64 functions
base64ToBytes :: B.ByteString -> B.ByteString
base64ToBytes  = B64.decodeLenient

bytesToBase64 :: B.ByteString -> B.ByteString
bytesToBase64 = B64.encode

-- Base16 (hex) functions
hexToBytes :: B.ByteString -> B.ByteString
hexToBytes = fst . B16.decode

bytesToHex :: B.ByteString -> B.ByteString
bytesToHex = B16.encode

-- String to Bytes Conversions
type UTF8String = String

bytesToString :: B.ByteString -> UTF8String
bytesToString = T.unpack . TE.decodeUtf8With (\_ _ -> Just '�')

stringToBytes :: UTF8String -> B.ByteString
stringToBytes = TE.encodeUtf8 . T.pack
