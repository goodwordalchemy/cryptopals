import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Lib

isSubstring :: B.ByteString -> B.ByteString -> Bool
isSubstring needle haystack = B.length match > 0
    where (_, match) = B.breakSubstring needle haystack

subStringAtIndexIsRepeated :: B.ByteString -> Int -> Bool
subStringAtIndexIsRepeated text idx= isSubstring front back
    where (front, back) = B.splitAt 16 usableText
          (_, usableText) = B.splitAt idx text

detectECB :: B.ByteString -> Bool
detectECB text = any id
               $ map (subStringAtIndexIsRepeated text) 
               $ possibleStartPoints
    where possibleStartPoints = [0..lastStartPoint]
          lastStartPoint = (B.length text) - (2*16)

findECBLines :: [B.ByteString] -> [Int]
findECBLines candidates = map fst
                        $ filter snd
                        $ zip [0..] 
                        $ map detectECB candidates

filename :: String
filename = "data/8.txt"

loadEncryptedFile :: IO [B.ByteString]
loadEncryptedFile = do
    contents <- B.readFile filename
    return $ map Lib.hexToBytes $ BC.lines contents

main :: IO ()
main = do
    lines <- loadEncryptedFile
    let detected = findECBLines lines
    print $ map (lines !!) detected
    
    
