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
xorWithLetter text letter = Lib.fixedXOR text 
                          $ BC.replicate (B.length text) letter

-- frequency analysis
expectedFrequencies :: Map.Map Char Float
expectedFrequencies = Map.fromList [ ('a', 11.682 / 100)
                                   , ('b', 4.434 / 100)
                                   , ('c', 5.238 / 100)
                                   , ('d', 3.174 / 100)
                                   , ('e', 2.799 / 100)
                                   , ('f', 4.027 / 100)
                                   , ('g', 1.642 / 100)
                                   , ('h', 4.200 / 100)
                                   , ('i', 7.294 / 100)
                                   , ('j', 0.511 / 100)
                                   , ('k', 0.456 / 100)
                                   , ('l', 2.415 / 100)
                                   , ('m', 3.826 / 100)
                                   , ('n', 2.284 / 100)
                                   , ('o', 7.631 / 100)
                                   , ('p', 4.319 / 100)
                                   , ('q', 0.222 / 100)
                                   , ('r', 2.826 / 100)
                                   , ('s', 6.686 / 100)
                                   , ('t', 15.978 / 100)
                                   , ('u', 1.183 / 100)
                                   , ('v', 0.824 / 100)
                                   , ('w', 5.497 / 100)
                                   , ('x', 0.045 / 100)
                                   , ('y', 0.763 / 100)
                                   , ('z', 0.045 / 100)
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
