import qualified Data.ByteString.Char8 as BC

import MD4
import qualified Lib

md4Device = md4PrefixMac key

sha1StateChunks 
    :: B.ByteString -> (Word32, Word32, Word32, Word32, Word32) 
sha1StateChunks text = (a, b, c, d, e)
    where
        [a, b, c, d, e] = map (iToW32 . Lib.intFromBigEndian32) 
                        $ (Lib.splitIntoChunks 4 text)
        iToW32 i = (fromIntegral i)::Word32

getLengthExtensionHash :: B.ByteString -> B.ByteString -> B.ByteString
getLengthExtensionHash digest added = 
    Lib.strictBL $ bytestringDigest hashed'
    where
        hashed' = sha1FromState stateChunks (Lib.lazyB added)
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

testLengthExtension :: IO ()
testLengthExtension = do
    let orig = BC.pack "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
        hashed = md4Device orig
        added = BC.pack ";admin=true"
        
        attackMsg = getForgedMessage 11 orig added
        added' = snd $ B.splitAt (128) $ sha1Pad $ (BC.replicate 11 'A') `B.append` attackMsg
        attackHash = getLengthExtensionHash hashed added'

    print $ "orig hash => " ++ (show $ Lib.bytesToHex hashed)
    print $ "attack msg => " ++ show attackMsg
    print $ "attack hash => " ++ (show $ Lib.bytesToHex attackHash)
    print $ "hash of forged msg =>" ++ (show $ Lib.bytesToHex (sha1Device attackMsg))

challenge29 :: Bool
challenge29 = 
    let orig = BC.pack "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
        hashed = sha1Device orig
        added = BC.pack ";admin=true"
        
        attackMsg = getForgedMessage 11 orig added
        added' = snd $ B.splitAt (128) $ sha1Pad $ (BC.replicate 11 'A') `B.append` attackMsg
        attackHash = getLengthExtensionHash hashed added'
    in attackHash == (sha1Device attackMsg)
