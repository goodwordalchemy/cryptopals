module Challenge6(challenge6) where

import Data.Bits(popCount)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.List(sortOn)
import Data.Tuple(snd)
import Data.Word(Word8)
import Debug.Trace

import Lib

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

avgNormedHammingSizeOffset :: Int -> B.ByteString -> Float
avgNormedHammingSizeOffset size text = sumChunks / nTrials
    where 
        sumChunks = sum $ map (normalizedHammingSizeOffset size) chunks
        nTrials  = fromIntegral $ length chunks
        chunks = splitIntoChunks (size*2) text
        

bestKeySizesWithScore :: B.ByteString -> [(Int, Float)]
bestKeySizesWithScore text = take 10
                  . sortOn snd 
                  $ sizesAndScores
    where
        sizesAndScores :: [(Int, Float)]
        sizesAndScores = mapWithOrig hammingAtKeySize
                       $ keySizesToTest
        
        hammingAtKeySize  = (flip avgNormedHammingSizeOffset) text
        keySizesToTest = [2..(min 40 $ (B.length text) `div` 2)]

bestKeySizes :: B.ByteString -> [Int]
bestKeySizes = map fst . bestKeySizesWithScore


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
filename = "../data/6.txt"

loadEncryptedFile :: IO B.ByteString
loadEncryptedFile = do
    contents <- B.readFile filename
    return $ Lib.base64ToBytes contents

testFindingKeySize :: IO ()
testFindingKeySize = do
    let plainText = Lib.stringToBytes "\
        \Burning 'em, if you ain't quick and nimble\n\
        \I go crazy when I hear a cymbal"
        expectedCipherText1 = Lib.stringToBytes "\
        \0b3637272a2b2e63622c2e69692a23693a2a3c\
        \6324202d623d63343c2a26226324272765272a\
        \282b2f20430a652e2c652a3124333a653e2b20\
        \27630c692b20283165286326302e27282f"

        expectedCipherText2 = Lib.stringToBytes "\
        \637272a2b2e63622c2e69692a23693a2a3c\
        \6324202d623d63343c2a26226324272765272a\
        \282b2f20430a652e2c652a3124333a653e2b20\
        \27630c692b20283165286326302e27282f"
    putStr "This list should start with 3 -->"
    print $ bestKeySizesWithScore expectedCipherText1
    print $ bestKeySizesWithScore expectedCipherText2

testDecryption :: IO ()
testDecryption = do
    cipherText <- loadEncryptedFile
    let result = head $ bestDecryptions cipherText
    print result    

challenge6 :: IO [String]
challenge6 = do
    cipherText <- loadEncryptedFile
    let result = head $ bestDecryptions cipherText
    return $ take 2 . words $ Lib.bytesToString result


main :: IO ()
main = do
    putSep
    testHammingDistance
    putSep
    testHammingDistanceKeySizeOffset
    putSep 
    testFindingKeySize
    testDecryption
