import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Lib

findECBLines :: [B.ByteString] -> [Int]
findECBLines candidates = map fst
                        $ filter snd
                        $ zip [0..] 
                        $ map Lib.detectECB candidates

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
    print "This list should only have 1 element ==>"
    print $ map (lines !!) detected
    
    
