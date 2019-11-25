import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Digest.Pure.SHA(padSHA1)
import Debug.Trace

import qualified Lib

padLastBlock :: Int -> B.ByteString -> B.ByteString
padLastBlock len block
  | len `mod` 64 < 56 = 
    let zeros = B.replicate nZeros 0
    in B.concat [ block'
                , zeros
                , encodedLength
                ]

  | otherwise = 
      let zeros = B.replicate (64 + nZeros) 0
      in B.concat [ block' 
                  , zeros
                  , encodedLength
                  ]
    where
        nZeros =  55 - (len `mod` 64)
        encodedLength = Lib.bigEndian64 (8*len)
        block' = block `B.append` (B.singleton 0x80)

sha1PadHelper :: Int -> B.ByteString -> B.ByteString
sha1PadHelper len text 
  | B.length text < 64 = padLastBlock len text
  | otherwise = B.append front $ sha1PadHelper len back
    where
        (front, back) = B.splitAt 64 text


sha1Pad :: B.ByteString -> B.ByteString
sha1Pad text = sha1PadHelper (B.length text) text

paddingIsSame :: String -> Bool
paddingIsSame text =
    let bs = BC.pack text
        myPadding = sha1Pad bs
        theirPadding = Lib.strictBL . padSHA1 . Lib.lazyB $ bs
    in 
    myPadding == theirPadding
    
testSha1MdPad :: [(String, Bool)]
testSha1MdPad =  Lib.mapWithOrig paddingIsSame 
    [ 
    "hello"
    , "This is a much longer string"
    , replicate 600 'A'
    , replicate 55 'D'
    , replicate 56 'B'
    , replicate 63 'C'
    , replicate 64 'E'
    , replicate 65 'F'
    ]

main :: IO ()
main = do
    print testSha1MdPad
