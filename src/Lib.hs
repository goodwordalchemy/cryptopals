module Lib (
    base64ToBytes,
    bytesToBase64,
    hexToBytes,
    bytesToHex,
    bytesToString,
    stringToBytes,
    hexStringToBytes,
    charToWord8,
    word8ToChar,
    fixedXOR,
    xorWithLetter,
    sortedLetterScores,
    repeatingXOR,
    mostLikelyXorKey
) where

import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import Data.Char(isLetter, toLower, isControl, chr)
import Data.List(groupBy, sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple(snd)
import Data.Word(Word8)
import Debug.Trace

-- Base64 functions
base64ToBytes :: B.ByteString -> B.ByteString
base64ToBytes  = B64.decodeLenient

bytesToBase64 :: B.ByteString -> B.ByteString
bytesToBase64 = B64.encode

-- Base16 (hex) functions
hexToBytes :: B.ByteString -> B.ByteString
hexToBytes = fst . B16.decode

bytesToHex :: B.ByteString -> B.ByteString
bytesToHex = B16.encode

hexStringToBytes :: String -> B.ByteString
hexStringToBytes = hexToBytes . stringToBytes 

-- String to Bytes Conversions

bytesToString :: B.ByteString -> String
bytesToString = T.unpack . TE.decodeUtf8With (\_ _ -> Just '�')

stringToBytes :: String -> B.ByteString
stringToBytes = TE.encodeUtf8 . T.pack

charToWord8 :: Char -> Word8
charToWord8 = B.head . TE.encodeUtf8 . T.singleton

word8ToChar :: Word8 -> Char
word8ToChar = head . T.unpack . TE.decodeUtf8 . B.singleton

-- xor *-cryption
fixedXOR :: B.ByteString -> B.ByteString -> B.ByteString
fixedXOR p ek = B.pack 
              $ B.zipWith xor ek p

xorWithLetter :: B.ByteString -> Char -> B.ByteString
xorWithLetter text letter = fixedXOR text 
                          $ BC.replicate (B.length text) letter

repeatedByteString :: Int -> B.ByteString -> B.ByteString -> B.ByteString
repeatedByteString 0 bs acc = acc
repeatedByteString n bs acc = result
    where result = repeatedByteString (n-1) bs (B.append bs acc)

repeatingXOR :: B.ByteString -> B.ByteString -> B.ByteString
repeatingXOR text key = fixedXOR text rKey
    where 
        rKey = repeatedByteString nTimes key key
        nTimes = (B.length text) `div` (B.length key) + 1

-- frequency analysis
expectedFrequencies :: Map.Map Char Float
expectedFrequencies = Map.fromList [ ('a', 8.167)
                                   , ('b', 1.492)
                                   , ('c', 2.782)
                                   , ('d', 4.253)
                                   , ('e', 2.702)
                                   , ('f', 2.228)
                                   , ('g', 2.015)
                                   , ('h', 6.094)
                                   , ('i', 6.966)
                                   , ('j', 0.153)
                                   , ('k', 0.772)
                                   , ('l', 4.025)
                                   , ('m', 2.406)
                                   , ('n', 6.749)
                                   , ('o', 7.507)
                                   , ('p', 1.929)
                                   , ('q', 0.095)
                                   , ('r', 5.987)
                                   , ('s', 6.327)
                                   , ('t', 9.056)
                                   , ('u', 2.758)
                                   , ('v', 0.978)
                                   , ('w', 2.360)
                                   , ('x', 0.150)
                                   , ('y', 1.974)
                                   , ('z', 0.074)
                                   , (' ', 10)
                                   ]

lookupFrequency :: Char -> Float
lookupFrequency letter = case Map.lookup letter expectedFrequencies of 
        Just x -> x
        Nothing -> 0

nOccurances :: String -> [(Char, Int)]
nOccurances = map (\l -> (head l, length l)) . groupBy (==) . sort

getLetterScoreFunc :: Int -> Map.Map Char Int -> (Char -> Float -> Float -> Float)
getLetterScoreFunc textLen obsCounts = (\letter freq acc -> 
    let
        obs = fromIntegral $ Map.findWithDefault 0 letter obsCounts 
        exp = freq * (fromIntegral textLen)
    in (exp-obs)**2/exp
                                      )


chiSquaredFreqScore :: B.ByteString -> Float
chiSquaredFreqScore text = Map.foldrWithKey letterScore 0 expectedFrequencies
    where
        letterScore = getLetterScoreFunc textLen observedFrequencies
        observedFrequencies = Map.fromList
                           $ filter letterInExpectedFrequencies
                           . nOccurances 
                           . map toLower
                           . bytesToString $ text
        letterInExpectedFrequencies (l, _) = Map.member l expectedFrequencies
        textLen = B.length text


scoreString :: B.ByteString -> Float
scoreString text = chiSquaredFreqScore text

snd' :: (a, b, c) -> b
snd' (a, b, c) = b

possibleLetters :: [Char]
possibleLetters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

sortedLetterScores :: B.ByteString -> [(Char, Float, String)]
sortedLetterScores text = sortOn snd' lettersWithScores
    where
        lettersWithScores :: [(Char, Float, String)]
        lettersWithScores = map letterWithScore possibleLetters

        letterWithScore :: Char -> (Char, Float, String)
        letterWithScore l = let dt = decodeText l 
                            in (l, scoreString dt, Lib.bytesToString dt)

        decodeText l = xorWithLetter text l

mostLikelyXorKey :: B.ByteString -> Char
mostLikelyXorKey text = key
    where (key, _, _) = head $ sortedLetterScores text
