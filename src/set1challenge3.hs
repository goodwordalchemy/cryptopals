import qualified Data.ByteString as B

import Lib

cipherText :: String
cipherText  = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

cipherTextAsBS :: B.ByteString
cipherTextAsBS = Lib.hexStringToBytes cipherText


main = do
    mapM_ print $ take 5 $ Lib.sortedLetterScores cipherTextAsBS
