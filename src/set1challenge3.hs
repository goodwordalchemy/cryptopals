import Data.Bits(xor)
import qualified Data.ByteString as B
import Data.Char(toLower)
import Data.List(sortOn)
import qualified Data.Map.Strict as Map
import Data.Tuple(snd)
import Data.Word

import Lib

type EncodedByteString = B.ByteString

cipherText :: String
cipherText  = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

cipherTextAsBS :: B.ByteString
cipherTextAsBS = Lib.hexStringToBytes cipherText

characterFrequencies :: Map.Map Char Float
characterFrequencies = Map.fromList [
    ('a', 8.167),
    ('b', 1.492),
    ('c', 2.782),
    ('d', 4.253),
    ('e', 12.702),
    ('f', 2.228),
    ('g', 2.015),
    ('h', 6.094),
    ('i', 6.966),
    ('j', 0.153),
    ('k', 0.772),
    ('l', 4.025),
    ('m', 2.406),
    ('n', 6.749),
    ('o', 7.507),
    ('p', 1.929),
    ('q', 0.095),
    ('r', 5.987),
    ('s', 6.327),
    ('t', 9.056),
    ('u', 2.758),
    ('v', 0.978),
    ('w', 2.360),
    ('x', 0.150),
    ('y', 1.974),
    ('z', 0.07)]

xorWithLetter :: EncodedByteString -> Char -> EncodedByteString
xorWithLetter text letter = B.map xorWord text
    where
        xorWord x = x `xor` (charToWord8 letter)

scoreLetter :: Char -> Float
scoreLetter l = if (toLower l) `elem` "etaoin shrdlu" then 1 else 0
-- scoreLetter l = (Map.findWithDefault 0 l characterFrequencies) ** 2

letterScore :: EncodedByteString -> Char -> Float
letterScore text letter = sum $ map scoreLetter xoredChars
    where
        xoredChars = map (toLower . word8ToChar) xored
        xored = B.unpack $ xorWithLetter text letter

sortedLetterScores :: EncodedByteString -> [(Char, Float)]
sortedLetterScores text = sortOn snd lettersWithScores
    where
        lettersWithScores :: [(Char, Float)]
        lettersWithScores = map letterWithScore ['a'..'z']

        letterWithScore :: Char -> (Char, Float)
        letterWithScore l = (l, letterScore text l)
