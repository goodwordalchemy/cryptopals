import Data.Bits(popCount)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List(sortOn)
import Data.Tuple(snd)
import Data.Word(Word8)
import Debug.Trace

import Lib

mapWithOrig :: (a -> b) -> [a] -> [(a, b)]
mapWithOrig func = map (\a -> (a, func a)) 

hamming :: B.ByteString -> B.ByteString -> Int
hamming a b = B.foldl numSetBits 0 $ Lib.fixedXOR a b
    where 
        numSetBits :: Int -> Word8 -> Int
        numSetBits acc letter = acc + (popCount letter)

hammingKeySizeOffset :: Int -> B.ByteString -> Int
hammingKeySizeOffset size text = hamming first second
    where
        (first, rest) = B.splitAt size text
        (second, _) = B.splitAt size rest

normalizedHammingSizeOffset :: Int -> B.ByteString -> Float
normalizedHammingSizeOffset size text = h / s
    where
        h = fromIntegral $ hammingKeySizeOffset size text
        s = fromIntegral size

bestKeySizesWithScore :: B.ByteString -> [(Int, Float)]
bestKeySizesWithScore text = take 5
                  $ sortOn snd 
                  $ sizesAndScores
    where
        sizesAndScores :: [(Int, Float)]
        sizesAndScores = mapWithOrig hammingAtKeySize
                       $ keySizesToTest
        
        hammingAtKeySize  = (flip normalizedHammingSizeOffset) text
        keySizesToTest = [2..40]

bestKeySizes :: B.ByteString -> [Int]
bestKeySizes = map fst . bestKeySizesWithScore


splitIntoChunks :: Int -> B.ByteString -> [B.ByteString]
splitIntoChunks n text 
    | text == B.empty = []
    | B.length text < n = []
    | otherwise = front : splitIntoChunks n rest
    where (front, rest) = B.splitAt n text

solveBlocks :: [B.ByteString] -> [Word8]
solveBlocks = map (Lib.charToWord8 . Lib.mostLikelyXorKey)

mostLikelyKeyAtKeySize :: Int -> B.ByteString -> B.ByteString
mostLikelyKeyAtKeySize size text = key
    where key = B.pack 
              . solveBlocks
              . B.transpose
              $ splitIntoChunks size text

mostLikelyKeys :: B.ByteString -> [B.ByteString]
mostLikelyKeys text = map keyAtSizeN sizesToTry
    where sizesToTry = bestKeySizes text
          keyAtSizeN = (flip mostLikelyKeyAtKeySize) text

decryptWithKey :: B.ByteString -> B.ByteString -> B.ByteString
decryptWithKey text key = Lib.repeatingXOR text key

bestDecryptions :: B.ByteString -> [B.ByteString]
bestDecryptions text = map decryptWithKey 
                     $ mostLikelyKeys text
    where 
        decryptWithKey :: B.ByteString -> B.ByteString
        decryptWithKey = Lib.repeatingXOR text


--- Testing functions
putSep :: IO ()
putSep = do
    putStrLn "=========================================="

testHammingDistance :: IO ()
testHammingDistance = do
    putStrLn "Testing hamming distance..."
    let a = Lib.stringToBytes "this is a test"
        b = Lib.stringToBytes "wokka wokka!!!"
        result = hamming a b
    putStrLn $ "This should be 37 ==> " ++ show result


testHammingDistanceKeySizeOffset :: IO ()
testHammingDistanceKeySizeOffset = do
    putStrLn "Testing hamming distance at keysize offset..."
    let text = Lib.stringToBytes "this is a testwokka wokka!!!iskkdkdk"
        result = hammingKeySizeOffset 14 text
    putStrLn $ "This should be 37 ==> " ++ show result

filename :: String
filename = "data/6.txt"

loadEncryptedFile :: IO B.ByteString
loadEncryptedFile = do
    contents <- B.readFile filename
    return $ Lib.base64ToBytes 
           . BC.concat 
           . BC.lines $ contents 


testDecryption :: IO ()
testDecryption = do
    cipherText <- loadEncryptedFile
    let results = bestDecryptions cipherText
    print results    

main :: IO ()
main = do
    putSep
    testHammingDistance
    putSep
    testHammingDistanceKeySizeOffset
    putSep 
    testDecryption
