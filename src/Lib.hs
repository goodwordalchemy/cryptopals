module Lib ( mapWithOrig
           , splitIntoChunks
           , chunks16
           , findRepetitionIndex
           , base64ToBytes
           , bytesToBase64
           , hexToBytes
           , bytesToHex
           , bytesToString
           , stringToBytes
           , hexStringToBytes
           , charToWord8
           , word8ToChar
           , fixedXOR
           , xorWithLetter
           , sortedLetterScores
           , repeatingXOR
           , mostLikelyXorKey
           , padToLength
           , padToMultiple
           , detectECB
           , initAES128
           , ecbDecryption
           , ecbEncryption
           , cbcDecryption
           , cbcEncryption
           , getRandomAESKey
           , getRandomLetterStream
           , randomByteString
           ) where

import Crypto.Cipher
import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import Data.Char(isLetter, toLower, isControl, chr, isPunctuation)
import Data.List(groupBy, sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Tuple(snd)
import Data.Word(Word8)
import Debug.Trace
import System.Random

mapWithOrig :: (a -> b) -> [a] -> [(a, b)]
mapWithOrig func = map (\a -> (a, func a)) 

splitIntoChunks :: Int -> B.ByteString -> [B.ByteString]
splitIntoChunks n text 
    | text == B.empty = []
    | B.length text < n = [text]
    | otherwise = front : splitIntoChunks n rest
    where (front, rest) = B.splitAt n text

chunks16 :: B.ByteString -> [B.ByteString]
chunks16 = Lib.splitIntoChunks 16

subStringIndex :: B.ByteString -> B.ByteString -> Int
subStringIndex needle haystack = B.length garbage
    where (garbage, match) = B.breakSubstring needle haystack

isSubstring :: B.ByteString -> B.ByteString -> Bool
isSubstring needle haystack = idx < B.length haystack 
    where
        idx = subStringIndex needle haystack

subStringAtIndexIsRepeated :: B.ByteString -> Int -> Bool
subStringAtIndexIsRepeated text idx = isSubstring front back
    where (front, back) = B.splitAt 16 usableText
          (_, usableText) = B.splitAt idx text

findRepetitionIndices :: B.ByteString -> [Int]
findRepetitionIndices text = map fst
               $ filter snd 
               $ mapWithOrig (subStringAtIndexIsRepeated text) 
               $ possibleStartPoints
    where possibleStartPoints = [0..lastStartPoint]
          lastStartPoint = (B.length text) - (2*16)

findRepetitionIndex :: B.ByteString -> Maybe Int
findRepetitionIndex text = if (length idxs) > 0
                               then Just (idxs !! 0)
                               else Nothing
    where idxs = findRepetitionIndices text


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
expectedFrequencies = Map.fromList [ ('a', 0.0651738)
                                   , ('b', 0.0124248)
                                   , ('c', 0.0217339)
                                   , ('d', 0.0349835)
                                   , ('e', 0.1041442)
                                   , ('f', 0.0197881)
                                   , ('g', 0.0158610)
                                   , ('h', 0.0492888)
                                   , ('i', 0.0558094)
                                   , ('j', 0.0009033)
                                   , ('k', 0.0050529)
                                   , ('l', 0.0331490)
                                   , ('m', 0.0202124)
                                   , ('n', 0.0564513)
                                   , ('o', 0.0596302)
                                   , ('p', 0.0137645)
                                   , ('q', 0.0008606)
                                   , ('r', 0.0497563)
                                   , ('s', 0.0515760)
                                   , ('t', 0.0729357)
                                   , ('u', 0.0225134)
                                   , ('v', 0.0082903)
                                   , ('w', 0.0171272)
                                   , ('x', 0.0013692)
                                   , ('y', 0.0145984)
                                   , ('z', 0.0007836)
                                   , (' ', 0.1918182)]

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

isControl' :: Char -> Bool
isControl' c = isControl c && c /= '\n' || '\65533' == c

isPunctuation' :: Char -> Bool
isPunctuation' c = isPunctuation c && c `notElem` [',','.','\'']

chiSquaredFreqScore :: B.ByteString -> Float
chiSquaredFreqScore text = controlCharCoef + punctCoef + chiSquared
    where
        controlCharCoef = 1000 * (textLength / nNonControlChars)
        nNonControlChars = fromIntegral $ B.length nonControlChars

        punctCoef = (nPunct / textLength)*200
        nPunct = fromIntegral . B.length
               $ BC.filter isPunctuation' text

        textLength = fromIntegral $ B.length text

        chiSquared = Map.foldrWithKey letterScore 0 expectedFrequencies
        letterScore = getLetterScoreFunc textLen observedFrequencies
        observedFrequencies = Map.fromList
                           $ filter letterInExpectedFrequencies
                           . nOccurances 
                           . map toLower
                           . bytesToString 
                           $ nonControlChars
        nonControlChars = BC.filter (not . isControl') $ text
        letterInExpectedFrequencies (l, _) = Map.member l expectedFrequencies
        textLen = B.length text


scoreString :: B.ByteString -> Float
scoreString text = chiSquaredFreqScore text

snd' :: (a, b, c) -> b
snd' (a, b, c) = b

possibleLetters :: [Char]
-- possibleLetters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
possibleLetters = map chr [0..255]

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

-- Padding tools
padToLength :: B.ByteString -> Int -> B.ByteString
padToLength text size = B.append text padding
    where padding = B.replicate diff (fromIntegral diff)
          diff = size - B.length text

padToMultiple :: B.ByteString -> Int -> B.ByteString
padToMultiple text ofM 
    | B.length text == ofM = text
    | otherwise = padToLength text lengthToPad
    where 
        lengthToPad = if floor == textLength 
                      then textLength 
                      else (q+1) * ofM
        floor = q * ofM
        q = textLength `div` ofM
        textLength = B.length text


-- AES tools
detectECB :: B.ByteString -> Bool
detectECB text = any id
               $ map (subStringAtIndexIsRepeated text) 
               $ possibleStartPoints
    where possibleStartPoints = [0..lastStartPoint]
          lastStartPoint = (B.length text) - (2*16)


initAES128 :: B.ByteString -> AES128
initAES128 = either (error . show) cipherInit . makeKey

ecbDecryption :: AES128 -> B.ByteString -> B.ByteString
ecbDecryption ctx cipherText = ecbDecrypt ctx cipherText

ecbEncryption :: AES128 -> B.ByteString -> B.ByteString
ecbEncryption ctx plainText = ecbEncrypt ctx paddedPlainText
    where paddedPlainText = padToMultiple plainText 16

cbcEncryptionStep 
    :: AES128 
    -> B.ByteString 
    -> B.ByteString 
    -> B.ByteString
    -> B.ByteString
cbcEncryptionStep ctx prevCipher plainText acc
    | plainText == B.empty = acc
    | otherwise = cbcEncryptionStep ctx curCipher nextPlain cipherSoFar
    where
        cipherSoFar = B.append acc curCipher

        curCipher = ecbEncryption ctx xored
        xored = fixedXOR prevCipher curPlainPadded
        curPlainPadded = padToMultiple curPlain 16
        
        (curPlain, nextPlain) = B.splitAt 16 plainText

cbcEncryption :: AES128 -> B.ByteString -> B.ByteString -> B.ByteString
cbcEncryption ctx iv text
    | B.length iv /= 16 = error "Initialization vector must have length 16"
    | otherwise = cbcEncryptionStep ctx iv text B.empty

cbcDecryption :: AES128 -> B.ByteString -> B.ByteString -> B.ByteString
cbcDecryption ctx iv cipherText
    | B.length iv /= 16 = error "Initialization vector must have length 16"
    | otherwise = B.concat plainTexts
    where 
        plainTexts = zipWith fixedXOR decrypteds
                   $ iv : chunks
        decrypteds = map (ecbDecryption ctx) chunks
        chunks = splitIntoChunks 16 cipherText

randomByteString :: [Word8] -> Int -> (B.ByteString, [Word8])
randomByteString rLetters n = (result, rest)
    where result = B.pack these
          (these, rest) = splitAt n rLetters

getRandomLetterStream :: StdGen -> ([Word8], StdGen)
getRandomLetterStream g = (randomRs (0, 255) g, g)

getRandomAESKey :: StdGen -> (B.ByteString, StdGen)
getRandomAESKey gen = (key, gen')
    where 
        (key, _) = randomByteString letterStream 16
        (letterStream, gen') = getRandomLetterStream gen

