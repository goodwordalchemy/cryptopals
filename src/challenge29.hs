import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import Data.Digest.Pure.SHA(bytestringDigest, padSHA1, sha1, sha1FromState)
import Data.Word(Word32)
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

sha1StateChunks 
    :: B.ByteString -> (Word32, Word32, Word32, Word32, Word32) 
sha1StateChunks text = (a, b, c, d, e)
    where
        [a, b, c, d, e] = map (iToW32 . Lib.intFromBigEndian32) 
                        $ (Lib.splitIntoChunks 4 text)
        iToW32 i = (fromIntegral i)::Word32

getLengthExtensionHash :: B.ByteString -> B.ByteString -> B.ByteString
getLengthExtensionHash digest added = 
    Lib.strictBL $ (trace $ "attack hash:" ++ show hashed')$bytestringDigest hashed'
    where
        hashed' = (trace $ "state chunks:"++ show stateChunks ++", added: " ++ show added)$ sha1FromState stateChunks (Lib.lazyB added)
        stateChunks = sha1StateChunks digest

getForgedMessage 
    :: Int 
    -> B.ByteString 
    -> B.ByteString 
    -> B.ByteString
getForgedMessage keylen orig added = 
    let fakeKey = BC.replicate keylen 'A'
        padded = sha1Pad (fakeKey `B.append` orig)
        forge = padded `B.append` added
    in snd $ B.splitAt keylen forge

-- Testing

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

sha1Device :: B.ByteString -> B.ByteString
sha1Device text = 
    let key = BC.pack "sticky icky"
    in Lib.sha1KeyedMAC key text

testSha1FromState :: IO ()
testSha1FromState = do
    let real = bytestringDigest $ sha1 (BCL.pack "test123")
        initialState = (0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0)
        fake = bytestringDigest 
             $ sha1FromState initialState (BCL.pack "test123")
    print $ real == fake


testLengthExtension :: IO ()
testLengthExtension = do
    let orig = BC.pack "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
        hashed = sha1Device orig
        added = BC.pack ";admin=true"
        
        attackMsg = getForgedMessage 11 orig added
        added' = snd $ B.splitAt (128) $ sha1Pad $ (BC.replicate 11 'A') `B.append` attackMsg
        attackHash = getLengthExtensionHash hashed added'

    print $ "orig hash => " ++ (show $ Lib.bytesToHex hashed)
    print $ "attack msg => " ++ show attackMsg
    print $ "attack hash => " ++ (show $ Lib.bytesToHex attackHash)
    print $ "hash of forged msg =>" ++ (show $ Lib.bytesToHex (sha1Device attackMsg))

testLengthExtension2 :: IO ()
testLengthExtension2 = do
    let orig = BC.empty
        hashed = sha1Device orig
        added = BC.empty
        
        attackMsg = getForgedMessage 11 orig added
        attackHash = getLengthExtensionHash hashed added

    print $ "orig hash => " ++ (show $ Lib.bytesToHex hashed)
    print $ "attack msg => " ++ show attackMsg
    print $ "attack hash => " ++ (show $ Lib.bytesToHex attackHash)
    print $ "hash of forged msg =>" ++ (show $ Lib.bytesToHex (sha1Device attackMsg))

main :: IO ()
main = do
    -- print testSha1MdPad
    -- testSha1FromState
    testLengthExtension
