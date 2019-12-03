module Challenge30(challenge30) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word32)

import qualified MD4
import qualified Lib

md4StateChunks 
    :: B.ByteString -> (Word32, Word32, Word32, Word32) 
md4StateChunks text = (a, b, c, d)
    where
        [a, b, c, d] = map (iToW32 . Lib.intFromLittleEndian32) 
                        $ (Lib.splitIntoChunks 4 text)
        iToW32 i = (fromIntegral i)::Word32

getLengthExtensionHash :: B.ByteString -> B.ByteString -> B.ByteString
getLengthExtensionHash digest added = 
    hashed'
    where
        hashed' = MD4.md4FromState stateChunks added
        stateChunks = md4StateChunks digest

getForgedMessage 
    :: Int 
    -> B.ByteString 
    -> B.ByteString 
    -> B.ByteString
getForgedMessage keylen orig added = 
    let fakeKey = BC.replicate keylen 'A'
        padded = MD4.md4Prepare (fakeKey `B.append` orig)
        forge = padded `B.append` added
    in snd $ B.splitAt keylen forge

-- Testing
md4Device :: B.ByteString -> B.ByteString
md4Device text = 
    let key = BC.pack "sticky icky"
    in MD4.md4PrefixMac key text

testLengthExtension :: IO ()
testLengthExtension = do
    let orig = BC.pack "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
        hashed = md4Device orig
        added = BC.pack ";admin=true"
        
        attackMsg = getForgedMessage 11 orig added
        added' = snd $ B.splitAt (128) $ MD4.md4Prepare $ (BC.replicate 11 'A') `B.append` attackMsg
        attackHash = getLengthExtensionHash hashed added'

    print $ "orig hash => " ++ (show $ Lib.bytesToHex hashed)
    print $ "attack msg => " ++ show attackMsg
    print $ "attack hash => " ++ (show $ Lib.bytesToHex attackHash)
    print $ "hash of forged msg =>" ++ (show $ Lib.bytesToHex (md4Device attackMsg))


challenge30 :: Bool
challenge30 = 
    let orig = BC.pack "comment1=cooking%20MCs;userdata=foo;comment2=%20like%20a%20pound%20of%20bacon"
        hashed = md4Device orig
        added = BC.pack ";admin=true"
        
        attackMsg = getForgedMessage 11 orig added
        added' = snd $ B.splitAt (128) $ MD4.md4Prepare $ (BC.replicate 11 'A') `B.append` attackMsg
        attackHash = getLengthExtensionHash hashed added'
    in attackHash == (md4Device attackMsg)

main :: IO ()
main = do
    -- print testSha1MdPad
    -- testSha1FromState
    testLengthExtension
