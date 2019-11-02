import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(toLower, isLetter)
import Data.List(groupBy, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Tuple(snd)
import Data.Word

import Lib

cipherText :: String
cipherText  = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

cipherTextAsBS :: B.ByteString
cipherTextAsBS = Lib.hexStringToBytes cipherText


expectedFrequencies :: Map.Map Char Float
expectedFrequencies = Map.fromList [ ('a', 0.08167)
                                   , ('b', 0.01492)
                                   , ('c', 0.02782)
                                   , ('d', 0.04253)
                                   , ('e', 10.02702)
                                   , ('f', 0.02228)
                                   , ('g', 0.02015)
                                   , ('h', 0.06094)
                                   , ('i', 0.06966)
                                   , ('j', 0.00153)
                                   , ('k', 0.00772)
                                   , ('l', 0.04025)
                                   , ('m', 0.02406)
                                   , ('n', 0.06749)
                                   , ('o', 0.07507)
                                   , ('p', 0.01929)
                                   , ('q', 0.00095)
                                   , ('r', 0.05987)
                                   , ('s', 0.06327)
                                   , ('t', 0.09056)
                                   , ('u', 0.02758)
                                   , ('v', 0.00978)
                                   , ('w', 0.02360)
                                   , ('x', 0.00150)
                                   , ('y', 0.01974)
                                   , ('z', 0.0007)
                                   ]

lookupFrequency :: Char -> Float
lookupFrequency letter = case Map.lookup letter expectedFrequencies of 
        Just x -> x
        Nothing -> 0

nOccurances :: String -> [(Char, Int)]
nOccurances = map (\l -> (head l, length l)) . groupBy (==) . sort

letterScore :: (Char, Int) -> Float
letterScore (l, obsCount) = calc (expCount l) (fromIntegral obsCount)
    where
        expCount letter = lookupFrequency letter
        calc exp obs = ((exp - obs)**2) / exp

chiSquaredFreqScore :: B.ByteString -> Float
chiSquaredFreqScore = sum 
                    . map letterScore 
                    . filter (\(l, _) -> Map.member l expectedFrequencies)
                    . nOccurances 
                    . map toLower
                    . Lib.bytesToString

xorWithLetter :: B.ByteString -> Char -> B.ByteString
xorWithLetter text letter = Lib.fixedXOR text 
                          $ BC.replicate (B.length text) letter

scorePossibleXorKey :: B.ByteString -> Char -> Float
scorePossibleXorKey text key = chiSquaredFreqScore 
                             $ xorWithLetter text key

snd' :: (a, b, c) -> b
snd' (a, b, c) = b

sortedLetterScores :: B.ByteString -> [(Char, Float, String)]
sortedLetterScores text = sortOn snd' lettersWithScores
    where
        lettersWithScores :: [(Char, Float, String)]
        lettersWithScores = map letterWithScore (['a'..'z'] ++ ['A'..'Z'])

        letterWithScore :: Char -> (Char, Float, String)
        letterWithScore l = (l, scorePossibleXorKey text l, decodedText l)

        decodedText l = Lib.bytesToString $ xorWithLetter text l

main = do
    mapM_ print $ take 5 $ sortedLetterScores cipherTextAsBS
