import qualified Data.ByteString as B

import qualified Lib

challengePlainText :: IO B.ByteString
challengePlainText = Lib.base64ToBytes $ B.readFile "data/25.txt"

editBlocks 
    :: B.ByteString 
    -> Int
    -> [B.ByteString]
    -> [B.ByteString]
editBlocks key blockOffset (this:rest)
  | theseBlocks == [] = []
  | otherwise = this':rest'
    where 
        rest' = editBlocks key (blockOffset+1) rest
        this' = Lib.ecbEncryptWithKey key' this
        key' = Lib.ctrStepKey key blockOffset

prependOffset 
    :: B.ByteString 
    -> Int 
    -> Int 
    -> B.ByteString 
    -> B.ByteString 
    -> B.ByteString
prependOffset key blockOffset offsetInBlock firstCtBlock newtext = newtext'
    where
        newtext' = spliceIn `B.append` newtext
        (spliceIn, _) = B.splitAt offsetInBlock decryptedFirstBlock
        decryptedFirstBlock = Lib.ecbEncryptWithKey key' firstCtBlock
        key' = Lib.ctrStepKey key blockOffset
        

edit :: B.ByteString -> B.ByteString -> Int -> B.ByteString -> B.ByteString
edit ciphertext key offset newtext 
  | offset > B.length ciphertext = error "offset cannot be greater than length of ciphertext"
  | otherwise = newCiphertext
    where 
        newCiphertext = B.concat $ beforeBlocks ++ editedBlocks ++ afterBlocks

        editedBlocks = editBlocks 
                            theseBlocks 
                            key 
                            blockOffset 
                            newtextBlocks
        
        newtextBlocks = chunks16 newText'
        newtext' = prependOffset offsetInBlock (theseBlocks !! 0) newtext
        
        (theseBlocks, afterBlocks) = B.split nBlocks rest
        (beforeBlocks, rest) = B.splitAt blockOffset blocks
        blocks = Lib.chunks16 ciphertext

        nBlocks = offsetInBlock + (B.length newtext)
        blockOffset = offset `div` 16
        offsetInBlock = offset `mod` 16

