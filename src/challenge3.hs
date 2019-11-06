module Challenge3(challenge3) where

import qualified Data.ByteString as B

import Lib

cipherText :: String
cipherText  = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

cipherTextAsBS :: B.ByteString
cipherTextAsBS = Lib.hexStringToBytes cipherText

challenge3 :: Char
challenge3 = bestLetter
    where (bestLetter, _, _) = head $ Lib.sortedLetterScores cipherTextAsBS      

main = do
    mapM_ print $ take 5 $ Lib.sortedLetterScores cipherTextAsBS
