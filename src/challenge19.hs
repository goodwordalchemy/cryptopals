import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(chr, ord)
import Data.Word(Word8)

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

firstPass :: [B.ByteString] -> [B.ByteString]
firstPass targets = decrypted
    where
        decrypted = map (Lib.fixedXOR xorKey) targets
        xorKey = BC.pack $ map Lib.mostLikelyXorKey transposed
        transposed = B.transpose targets

secondPass :: [B.ByteString] -> [B.ByteString] -> [B.ByteString]
secondPass targets firstPassResults = B.transpose substituted
    where
        substituted = performSubs substitutions transposed'

        performSubs [] soFar = soFar
        performSubs ((idx, letter):xs) soFar = 
            performSubs xs (swapAtIndex soFar idx (Lib.xorWithLetter 
                                                    (transposed !! idx) 
                                                    letter))

        transposed' = B.transpose firstPassResults
        transposed  = B.transpose targets

        substitutions = map colNumKeyPairs rawSubstitions

        colNumKeyPairs (targetNum, colNum, knownLetter) = 
            (colNum, word8ToChar $ xor
                                    (ctLetter targetNum colNum) 
                                    (charToWord8 knownLetter))

        ctLetter row col = targets !! row `B.index` col

        rawSubstitions = [ (0, 20, 'l') -- (target #, col #, knownLetter)
                         ]

crackFixedNonceTargets :: [B.ByteString] -> [B.ByteString]
crackFixedNonceTargets targets = secondPass targets firstPassResults
    where
        firstPassResults = firstPass targets

challenge19 :: [B.ByteString]
challenge19 = crackFixedNonceTargets targets

main :: IO ()
main = do
    print challenge19
        
