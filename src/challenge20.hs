module Challenge20(challenge20) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Debug.Trace

import Challenge19(crackFixedNonceTargets)
import qualified Lib

filename :: String
filename = "data/20.txt"

getTargets :: IO [B.ByteString]
getTargets = do
    contents <- readFile filename
    let contentLines = lines $ contents
    return $ map (Lib.base64ToBytes . BC.pack) contentLines

c20Subs :: [(Int, Int, Char)]
c20Subs = [(0, 0, 'I')]

challenge20Results :: IO [B.ByteString]
challenge20Results = do
    targets <- getTargets
    let (_, results) = crackFixedNonceTargets targets c20Subs []

    return results

challenge20 :: IO Bool
challenge20 = do
    results <- challenge20Results
    let lastLast = BC.last . last $ results
        lastFirst = BC.head . last $ results
    return $ (lastLast == 'a') && (lastFirst == 'A')

main :: IO ()
main = do
    results <- challenge20Results
    mapM_ print (zip [0..] results)
    
