import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List(sortOn, map)
import Debug.Trace
import System.IO

import Lib

type ScoreEntry = (Char, Float, String)

snd' :: (a, b, c) -> b
snd' (a, b, c) = b

mostLikelyPlaintexts :: [B.ByteString] -> [ScoreEntry]
mostLikelyPlaintexts candidates = sortOn (snd') allScores
    where allScores = concat 
                    $ map Lib.sortedLetterScores candidates
                    
fileName :: String
-- fileName = "data/4.txt" 
fileName = "data/test.txt"

loadCandidates :: IO [B.ByteString]
loadCandidates = do
    contents <- BC.readFile fileName
    return $ map Lib.hexToBytes $ BC.lines contents
    
main = do
    candidates <- loadCandidates
    let winners = mostLikelyPlaintexts candidates
    mapM_ print $ take 10 $ winners
