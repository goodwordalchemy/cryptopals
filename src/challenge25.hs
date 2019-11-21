module Challenge25(challenge25) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Debug.Trace

import qualified Lib

ctrStep' :: Int -> B.ByteString -> Int -> B.ByteString -> B.ByteString
ctrStep' nonce key count text = Lib.ctrStep ctx nonceBytes count text B.empty
    where
        ctx = Lib.initAES128 key
        nonceBytes = Lib.littleEndian64 nonce

splitQuotRem16 :: Int -> (Int, Int)
splitQuotRem16 n = (n `div` 16, n `mod` 16)

editBlocks 
    :: (B.ByteString, Int)
    -> Int
    -> [B.ByteString]
    -> [B.ByteString]
editBlocks (key, nonce) blockOffset (this:rest)
  | rest == [] = this':[]
  | otherwise = this':rest'
    where 
        rest' = editBlocks (key, nonce) (blockOffset+1) rest
        this' = ctrStep' nonce key blockOffset this

prependOffset 
    :: (B.ByteString, Int)
    -> Int
    -> B.ByteString 
    -> B.ByteString 
    -> B.ByteString
prependOffset (key, nonce) offset firstCtBlock newtext = newtext'
    where
        newtext' = spliceBefore `B.append` newtext `B.append` spliceAfter
        
        (_, spliceAfter) = B.splitAt (B.length newtext) rest
        (spliceBefore, rest) = B.splitAt offsetInBlock decryptedFirstBlock

        decryptedFirstBlock = ctrStep' nonce key blockOffset firstCtBlock

        (blockOffset, offsetInBlock) = splitQuotRem16 offset
        

edit 
    :: B.ByteString 
    -> (B.ByteString, Int)
    -> Int
    -> B.ByteString 
    -> B.ByteString
edit ciphertext keyNonce offset newtext 
  | offset > B.length ciphertext = error "offset cannot be greater than length of ciphertext"
  | otherwise = newCiphertext
    where 
        newCiphertext = B.concat $ beforeBlocks ++ editedBlocks ++ afterBlocks

        editedBlocks = editBlocks keyNonce blockOffset newtextBlocks
        
        newtextBlocks = Lib.chunks16 newtext'
        newtext' = prependOffset keyNonce offset (theseBlocks !! 0) newtext
        
        (theseBlocks, afterBlocks) = splitAt nBlocks rest
        (beforeBlocks, rest) = splitAt blockOffset blocks
        blocks = Lib.chunks16 ciphertext

        nBlocks = 1 + (offsetInBlock + (B.length newtext) - 1) `div` 16
        
        (blockOffset, offsetInBlock) = splitQuotRem16 offset

-- Attack
type EditFunc = Int -> B.ByteString -> B.ByteString

clipChunk :: Int -> B.ByteString -> B.ByteString
clipChunk offset text = chunk
    where
        (chunk, _) = B.splitAt 16 step1
        (_, step1) = B.splitAt offset text

attackEditHelper :: EditFunc -> B.ByteString -> Int -> B.ByteString
attackEditHelper editFunc ciphertext offset
  | offset >= B.length ciphertext = B.empty
  | otherwise = decrypted `B.append` decryptedRest
    where
        decryptedRest = attackEditHelper editFunc ciphertext (offset + 16)

        decrypted = unknown `Lib.fixedXOR` kCipher `Lib.fixedXOR` kPlain
        unknown = clipChunk offset ciphertext
        kCipher = clipChunk offset $ editFunc offset kPlain

        kPlain = BC.replicate 16 'A'

attackEdit :: EditFunc -> B.ByteString -> B.ByteString
attackEdit editFunc ciphertext = attackEditHelper editFunc ciphertext 0

-- Tests
testEdit :: IO ()
testEdit = do
    let pt = (BC.pack "my sexy plaintext, curves & all.")
        key = BC.replicate 16 'A'
        nonce = 42
        ct = Lib.getCTRDevice key nonce pt
        edited = edit ct (key,nonce) 3 (BC.pack "edit")
        nt = Lib.getCTRDevice key nonce edited
    print nt

encryptAndDecrypt :: B.ByteString -> B.ByteString
encryptAndDecrypt pt = 
    let key = BC.replicate 16 'A'
        nonce = 42
        ct = Lib.getCTRDevice key nonce pt
        editFunc = edit ct (key, nonce)
        
     in attackEdit editFunc ct

testAttackEdit :: IO ()
testAttackEdit = do
    let pt = (BC.pack "my sexy plaintext, curves & all.")
        attacked = encryptAndDecrypt pt
    print attacked

poem = "\"No Man Is an Island\" by John Donne\
\No man is an island, entire of itself;\
\every man is a piece of the continent,\
\a part of the main.\
\If a clod be washed away by the sea,\
\Europe is the less,\
\as well as if a promontory were,\
\as well as if a manor of thy friend's\
\or of thine own were.\
\Any man's death diminishes me,\
\because I am involved in mankind;\
\and therefore never send to know\
\for whom the bell tolls;\
\it tolls for thee."

challenge25 :: Bool
challenge25 = decrypted == raw
    where 
        decrypted = encryptAndDecrypt raw
        raw = BC.pack poem

--
main :: IO ()
main = do
    testEdit
    testAttackEdit
