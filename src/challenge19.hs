module Challenge19(challenge19) where

import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(chr, ord)
import Data.Word(Word8)
import Debug.Trace

import qualified Lib

targets :: [B.ByteString]
targets = map (Lib.base64ToBytes . BC.pack) rawTargets
    where 
        rawTargets = [ "SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ=="
                     , "Q29taW5nIHdpdGggdml2aWQgZmFjZXM="
                     , "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ=="
                     , "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4="
                     , "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk"
                     , "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
                     , "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ="
                     , "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
                     , "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU="
                     , "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl"
                     , "VG8gcGxlYXNlIGEgY29tcGFuaW9u"
                     , "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA=="
                     , "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk="
                     , "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg=="
                     , "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo="
                     , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
                     , "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA=="
                     , "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA=="
                     , "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA=="
                     , "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg=="
                     , "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw=="
                     , "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA=="
                     , "U2hlIHJvZGUgdG8gaGFycmllcnM/"
                     , "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w="
                     , "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4="
                     , "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ="
                     , "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs="
                     , "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA=="
                     , "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA=="
                     , "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4="
                     , "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA=="
                     , "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu"
                     , "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc="
                     , "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs"
                     , "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs="
                     , "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0"
                     , "SW4gdGhlIGNhc3VhbCBjb21lZHk7"
                     , "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw="
                     , "VHJhbnNmb3JtZWQgdXR0ZXJseTo="
                     , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
                     ]

charToWord8 :: Char -> Word8
charToWord8 c = (fromIntegral $ ord c)::Word8

word8ToChar :: Word8 -> Char
word8ToChar w = chr (fromIntegral w)

swapAtIndex :: [a] -> Int -> a -> [a]
swapAtIndex alist idx swapIn = before ++ [swapIn] ++ after
    where
        after = tail rest
        (before, rest) = splitAt idx alist

truncateToLength :: Int -> B.ByteString -> B.ByteString
truncateToLength n = fst . B.splitAt n

firstPartOfKey :: [B.ByteString] -> B.ByteString
firstPartOfKey targets = xorKey
    where
        xorKey = BC.pack $ map Lib.mostLikelyXorKey transposed
        transposed = B.transpose truncated

        truncated = map (truncateToLength shortestLength) targets
        shortestLength =  minimum $ map B.length targets

resultsForKey :: [B.ByteString] -> B.ByteString -> [B.ByteString]
resultsForKey targets key = map (Lib.fixedXOR key) targets

knownLetters :: [(Int, Int, Char)] -- (row, col, letter)
knownLetters = [ (10, 20, 'n')
               , (21, 21, 'f')
               , (21, 22, 'u')
               , (21, 23, 'l')
               , (14, 24, 'e')
               , (14, 25, 'r')
               , (14, 26, 'l')
               , (14, 27, 'y')
               , (29, 28, 'g')
               , (29, 29, 'h')
               , (0, 30, 'y')
               , (6, 31, 'd')
               , (27, 32, 'd')
               , (4, 33, 'e')
               , (4, 34, 'a')
               , (4, 35, 'd')
               , (37, 36, 'n')
               , (37, 37, ',')
               ]

getKeyLetter :: [B.ByteString] -> (Int, Int, Char) -> Word8
getKeyLetter targets (targetNum, colNum, knownLetter) = keyLetter
    where 
        keyLetter = ctLetter `xor` (charToWord8 knownLetter)
        ctLetter = targets !! targetNum `B.index` colNum

updatedKey
    :: [B.ByteString]
    -> [(Int, Int, Char)] 
    -> B.ByteString 
    -> B.ByteString
updatedKey targets knownLetters keySoFar = key
    where key = keySoFar `B.append` restOfKey
          restOfKey = B.pack $ map (getKeyLetter targets) knownLetters


challenge19 :: Bool
challenge19 = secondPassResults !! 0 == (BC.pack "I have met them at close of day")
    where 
        firstPassKey = firstPartOfKey targets
        firstPassResults = resultsForKey targets firstPassKey
        secondPassKey = updatedKey targets knownLetters firstPassKey 
        secondPassResults = resultsForKey targets secondPassKey
        curKeyLength = B.length secondPassKey

main :: IO ()
main = do
    let firstPassKey = firstPartOfKey targets
        firstPassResults = resultsForKey targets firstPassKey
        secondPassKey = updatedKey targets knownLetters firstPassKey 
        secondPassResults = resultsForKey targets secondPassKey
        curKeyLength = B.length secondPassKey

    putStrLn $ "results for key length: " ++ show curKeyLength ++ ":"
    mapM_ print (zip [0..] secondPassResults)
        

