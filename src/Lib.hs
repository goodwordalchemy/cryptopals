module Lib ( mapWithOrig
           , isSubstring
           , splitIntoChunks
           , chunks16
           , littleEndian32
           , littleEndian64
           , bigEndian32
           , bigEndian64
           , intFromBigEndian32
           , intFromLittleEndian32
           , nthChunk16
           , findRepetitionIndex
           , replaceAtIndex
           , replaceAtIndices
           , replaceBlock
           , replaceNth
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
           , pk7Pad
           , stripValidPadding
           , detectECB
           , initAES128
           , ecbDecryption
           , ecbEncryption
           , ecbEncryptWithKey
           , ecbDecryptWithKey
           , cbcDecryption
           , cbcEncryption
           , getRandomAESKey
           , getRandomLetterStream
           , randomByteString
           , getCTRDevice
           , ctrStep
           , getSecondsSinceEpoch
           , ctrStepKey
           , sha1KeyedMAC
           , lazyB
           , strictBL
           ) where

import Control.Concurrent(threadDelay)
import Crypto.Cipher
import Data.Bits(xor, shift, (.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import Data.Char(isLetter, toLower, isControl, chr, isPunctuation, ord)
import Data.Digest.Pure.SHA(bytestringDigest, sha1)
import Data.List(groupBy, sort, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock( getCurrentTime
                      , nominalDiffTimeToSeconds
                      , utctDayTime
                      , UTCTime )
import Data.Time.Clock.POSIX(utcTimeToPOSIXSeconds)
import Data.Time.LocalTime(timeToTimeOfDay, TimeOfDay)
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

nthChunk16 :: Int -> B.ByteString -> B.ByteString
nthChunk16 n text = (chunks16 text) !! n

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

replaceAtIndex :: Int -> Word8 -> B.ByteString -> B.ByteString
replaceAtIndex idx c text = (before `B.snoc` c) `B.append` rest
    where
        rest = B.tail at
        (before, at) = B.splitAt idx text

replaceAtIndices :: [(Int, Word8)] -> B.ByteString -> B.ByteString
replaceAtIndices [] text = text
replaceAtIndices ((idx, char):ics) text = replaceAtIndices ics newText
    where newText = replaceAtIndex idx char text

replaceBlock :: Int -> B.ByteString -> B.ByteString -> B.ByteString
replaceBlock n fullOrig replacement = replaced
    where
        blocks = Lib.chunks16 fullOrig
        (start, at) = splitAt n blocks
        (_, rest) = splitAt 1 at
        front = B.concat start
        back = B.concat rest
        replaced = B.concat [front, replacement, back]

replaceNth :: Int -> [a] -> a -> [a]
replaceNth n full replacement = replaced
    where 
        replaced = before ++ [replacement] ++ after
        (_, after) = splitAt 1 rest
        (before, rest) = splitAt n full

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
bytesToString = BC.unpack

stringToBytes :: String -> B.ByteString
stringToBytes = BC.pack

charToWord8 :: Char -> Word8
charToWord8 c = (fromIntegral $ ord c)::Word8

word8ToChar :: Word8 -> Char
word8ToChar w = chr (fromIntegral w)

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
isControl' c = c `notElem` ['\n', '\EOT', '\t'] 
             && (ord c <= 31 || ord c >= 127)

isPunctuation' :: Char -> Bool
isPunctuation' c = isPunctuation c && c `notElem` [',','.','\'']

chiSquaredFreqScore :: B.ByteString -> Float
chiSquaredFreqScore text = controlCharCoef + punctCoef + chiSquared
    where
        controlCharCoef = 100000 * (textLength / nNonControlChars)
        nNonControlChars = fromIntegral $ B.length nonControlChars

        punctCoef = (nPunct / textLength)*50
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
  | remainder == 0 = text
  | otherwise = padToLength text lengthToPad
    where 
        lengthToPad = textLength + (ofM - remainder)
        remainder = textLength `mod` ofM
        textLength = B.length text

pk7Pad :: B.ByteString -> B.ByteString
pk7Pad text 
  | B.length text `mod` 16 == 0 = text `B.append` (B.replicate 16 16)
  | otherwise = padToMultiple text 16

stripValidPadding :: B.ByteString -> Either String B.ByteString
stripValidPadding text = case fixedLastChunk of
                           Right c -> Right $ B.concat (firstChunks++[c])
                           Left msg -> Left msg
    where
        fixedLastChunk = stripValidPaddingChunk lastChunk
        lastChunk = lastChunks !! 0
        (firstChunks, lastChunks) = splitAt (length chunks - 1) chunks
        chunks = chunks16 text


stripValidPaddingChunk :: B.ByteString -> Either String B.ByteString
stripValidPaddingChunk text
  | B.length text /= 16 = Left "block is not 16 bytes long"
  | lastCharVal < 1 || lastCharVal > 16 = Left "last block must be padded"
  | correctPadding = Right unpaddedPart
  | otherwise = Left ("block is incorrectly padded" ++ show text)
  where 
      correctPadding = correctPaddingChar && allMatchLastChar 
      allMatchLastChar = B.all (== lastChar) paddedPart
      correctPaddingChar = lastCharVal == B.length paddedPart

      (unpaddedPart, paddedPart) = B.splitAt (16-lastCharVal) text
      lastCharVal = (fromIntegral $ lastChar)::Int
      lastChar = B.last text

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

ecbEncryptWithKey :: B.ByteString -> B.ByteString -> B.ByteString
ecbEncryptWithKey key text = ecbEncryption aes text
    where aes = initAES128 key

ecbDecryptWithKey :: B.ByteString -> B.ByteString -> B.ByteString
ecbDecryptWithKey key text = ecbDecryption aes text
    where aes = initAES128 key

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

data Endianness = Big | Little

xEndian :: Endianness -> Int -> Int -> B.ByteString
xEndian endianness nBits num = 
    B.pack 
    $ map (fromIntegral . last2BytesShifted)
    $ map (*8) byteIdxs

    where
        last2BytesShifted s = (num `shift` (-s)) .&. 0xFF
        byteIdxs = case endianness of
                     Big -> reverse [0..(nBytes-1)]
                     Little -> [0..(nBytes-1)]
        nBytes = nBits `div` 8

littleEndian :: Int -> Int -> B.ByteString
littleEndian = xEndian Little

littleEndian64 :: Int -> B.ByteString
littleEndian64 = littleEndian 64

littleEndian32 :: Int -> B.ByteString
littleEndian32 = littleEndian 32

bigEndian :: Int -> Int -> B.ByteString
bigEndian = xEndian Big

bigEndian64 :: Int -> B.ByteString
bigEndian64 = bigEndian 64

bigEndian32 :: Int -> B.ByteString
bigEndian32 = bigEndian 32

intFromBigEndian32 :: B.ByteString -> Int
intFromBigEndian32 text =
    foldl foldFunc 0 [0..3]
    where 
        foldFunc acc idx = 
            let letterVal = (w8AsInt (text `B.index` idx))
                shiftVal = 8*(3-idx)
            in acc + (letterVal `shift` shiftVal)
        w8AsInt w = (fromIntegral w)::Int

intFromLittleEndian32 :: B.ByteString -> Int
intFromLittleEndian32 text =
    foldl foldFunc 0 [0..3]
    where 
        foldFunc acc idx = 
            let letterVal = (w8AsInt (text `B.index` idx))
                shiftVal = 8*idx
            in acc + (letterVal `shift` shiftVal)
        w8AsInt w = (fromIntegral w)::Int

ctrStep 
    :: AES128
    -> B.ByteString 
    -> Int 
    -> B.ByteString 
    -> B.ByteString
    -> B.ByteString
ctrStep ctx nonceBytes count text acc
  | text == B.empty = acc
  | otherwise = ctrStep ctx nonceBytes (count+1) nextText soFar
    where
        soFar = acc `B.append` cipher
        cipher = fixedXOR encrypted thisText
        
        (thisText, nextText) = B.splitAt 16 text

        encrypted = ecbEncryption ctx ivCounterPair
        ivCounterPair = ctrStepKey nonceBytes count

ctrStepKey :: B.ByteString -> Int -> B.ByteString
ctrStepKey nonce count = B.append nonce $ littleEndian64 count

ctrEncryption :: AES128 -> Int -> B.ByteString -> B.ByteString
ctrEncryption ctx nonce text = ctrStep 
                                    ctx nonceBytes 0 text B.empty
    where nonceBytes = littleEndian64 nonce

ctrDecryption :: AES128 -> Int -> B.ByteString -> B.ByteString
ctrDecryption = ctrEncryption

getCTRDevice :: B.ByteString -> Int -> B.ByteString -> B.ByteString
getCTRDevice key 
  | B.length key /= 16 = error "AES128 keys must be 16 bytes"
  | otherwise = ctrEncryption aes
    where aes = initAES128 key


-- Time utils
timeOfDayFromUtc :: UTCTime -> TimeOfDay
timeOfDayFromUtc t = timeToTimeOfDay $ utctDayTime t

waitNSeconds :: Int -> IO ()
waitNSeconds n = threadDelay $ n * 10^6

secondsSinceEpoch :: UTCTime -> Int
secondsSinceEpoch = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

getSecondsSinceEpoch :: IO Int
getSecondsSinceEpoch = do
    now <- getCurrentTime
    return $ secondsSinceEpoch now

-- HMAC
lazyB :: B.ByteString -> BL.ByteString
lazyB = BL.fromStrict 

strictBL :: BL.ByteString -> B.ByteString
strictBL = B.concat . BL.toChunks

sha1KeyedMAC :: B.ByteString -> B.ByteString -> B.ByteString
sha1KeyedMAC key text = strictBL 
                      $ bytestringDigest 
                      $ sha1 (key' `BL.append` text')
    where
        key' = lazyB key
        text' = lazyB text

type HashFunc = B.ByteString -> B.ByteString

hmac :: HashFunc -> Int -> B.ByteString -> B.ByteString -> B.ByteString
hmac hashfunc blockSize key message =
    hashfunc (o_key_pad `B.append` (hashfunc (i_key_pad `B.append` message)))
    where
        key' = if B.length key > blockSize 
                  then hashfunc key
                  else if B.length key < blockSize
                    then key `B.append` (B.replicate (blockSize - B.length key) 0)
                    else key
        o_key_pad = (trace $ "key:" ++ show key')$Lib.xorWithLetter key' (chr 0x5c)
        i_key_pad = Lib.xorWithLetter key' (chr 0x36)

strictSha1 :: B.ByteString -> B.ByteString
strictSha1 = strictBL . bytestringDigest . sha1 . lazyB 

hmacSha1 :: B.ByteString -> B.ByteString -> B.ByteString
hmacSha1 = hmac strictSha1 64
