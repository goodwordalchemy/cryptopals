import qualified Data.ByteString as B

import qualified Lib

challengePlainText :: IO B.ByteString
challengePlainText = fmap Lib.base64ToBytes $ B.readFile "data/25.txt"

splitQuotRem16 :: Int -> (Int, Int)
splitQuotRem16 n = (n `div` 16, n `mod` 16)

editBlocks 
    :: B.ByteString 
    -> Int
    -> [B.ByteString]
    -> [B.ByteString]
editBlocks key blockOffset (this:rest)
  | rest == [] = this':[]
  | otherwise = this':rest'
    where 
        rest' = editBlocks key (blockOffset+1) rest
        this' = Lib.ecbEncryptWithKey key' this
        key' = Lib.ctrStepKey key blockOffset

prependOffset 
    :: B.ByteString 
    -> Int 
    -> B.ByteString 
    -> B.ByteString 
    -> B.ByteString
prependOffset key offset firstCtBlock newtext = newtext'
    where
        newtext' = spliceIn `B.append` newtext
        (spliceIn, _) = B.splitAt offsetInBlock decryptedFirstBlock
        decryptedFirstBlock = Lib.ecbEncryptWithKey key' firstCtBlock
        key' = Lib.ctrStepKey key blockOffset

        (blockOffset, offsetInBlock) = splitQuotRem16 offset
        

edit :: B.ByteString -> B.ByteString -> Int -> B.ByteString -> B.ByteString
edit ciphertext key offset newtext 
  | offset > B.length ciphertext = error "offset cannot be greater than length of ciphertext"
  | otherwise = newCiphertext
    where 
        newCiphertext = B.concat $ beforeBlocks ++ editedBlocks ++ afterBlocks

        editedBlocks = editBlocks key blockOffset newtextBlocks
        
        newtextBlocks = Lib.chunks16 newtext'
        newtext' = prependOffset key offset (theseBlocks !! 0) newtext
        
        (theseBlocks, afterBlocks) = splitAt nBlocks rest
        (beforeBlocks, rest) = splitAt blockOffset blocks
        blocks = Lib.chunks16 ciphertext

        nBlocks = offsetInBlock + (B.length newtext)
        
        (blockOffset, offsetInBlock) = splitQuotRem16 offset

