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
    sortedLetterScores,
) where

import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import Data.Char(isLetter, toLower)
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
