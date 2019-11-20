module Challenge24(challenge24) where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word8)
import Debug.Trace
import System.Random

import qualified Lib
import MersenneTwister( cloneMt
                      , getMtInts
                      , MTState
                      , seedMt )

doTrace = False

makeKeyStream :: MTState -> Int -> B.ByteString
makeKeyStream mt nLetters = B.concat
                          $ map Lib.littleEndian32
                          $ getMtInts nBytes mt
    where nBytes = nLetters `div` 4 + 1

mtEncrypt :: Int -> B.ByteString -> B.ByteString
mtEncrypt seed text
  | seed >= 2 ^ 16 = error "MT stream cipher must be seeded with 16 bit int"
  | otherwise = cipherText
    where
        cipherText = Lib.fixedXOR text keyStream
        keyStream = makeKeyStream mt $ B.length text
        mt = seedMt seed

type MTCipher = B.ByteString -> B.ByteString

randomBS :: StdGen -> Int -> Int -> B.ByteString
randomBS gen lowLength highLength = result
    where result = B.pack
                 $ take l 
                 $ randomRs (0, 255) gen'
          (l, gen') = randomR (lowLength, highLength) gen

getCipher :: StdGen -> Int -> MTCipher
getCipher gen seed text = mtEncrypt seed extended
    where 
        extended = garbage `B.append` text
        garbage = randomBS gen 0 100

bruteForceMtSeedHelper :: MTCipher -> Int -> Int -> Int
bruteForceMtSeedHelper cipher cur stop 
  | cur == stop = error "could not find seed"
  | theirs' == ours' = cur
  | otherwise = if (cur `mod` 1000 == 0) && doTrace
                   then trace ("\ndoing..." ++ show cur)$ result 
                   else result
    where
        result = bruteForceMtSeedHelper cipher (cur+1) stop

        ours' = extract ours
        theirs' = extract theirs
        
        extract :: B.ByteString -> B.ByteString
        extract = snd . B.splitAt prefixLength
        
        ours = mtEncrypt cur (prefix `B.append` knownText)
        prefix = BC.replicate prefixLength 'A'
        prefixLength = (B.length theirs) - (B.length knownText)

        theirs = cipher knownText
        
        knownText = BC.pack "12345678901234"


bruteForceMtSeed :: MTCipher -> Int
bruteForceMtSeed cipher = bruteForceMtSeedHelper cipher 0 (2^16)

-- password token
generateResetToken :: IO B.ByteString
generateResetToken = do
    timeSeed <- Lib.getSecondsSinceEpoch
    return $ intArrayToByteString
           $ getMtInts 16 (seedMt timeSeed)


intArrayToByteString :: [Int] -> B.ByteString
intArrayToByteString ints = B.pack 
                          $ map (\i -> (fromIntegral i)::Word8) ints

isMtTimeSeededTokenHelper :: B.ByteString -> Int -> Int -> Bool
isMtTimeSeededTokenHelper token cur stop
  | cur == stop = False
  | token == guess = True
  | otherwise = isMtTimeSeededTokenHelper token (cur+1) stop
    where
        guess = intArrayToByteString
              $ getMtInts tokenLength (seedMt cur)
        tokenLength = B.length token

isMtTimeSeededToken :: B.ByteString -> IO Bool
isMtTimeSeededToken token = do
    stop <- Lib.getSecondsSinceEpoch
    let start = stop - (60*60)
    return $ isMtTimeSeededTokenHelper token start (stop+1)


-- Tests
challenge24 :: Bool
challenge24 = guess == seed
    where 
        seed = 2
        cipher = getCipher (mkStdGen seed) seed
        guess = bruteForceMtSeed cipher


testStreamCipher :: IO ()
testStreamCipher = do
    let text = BC.pack "My shoes are awefully shiney"
        cipherText = mtEncrypt 42 text
        result = mtEncrypt 42 cipherText
    print $ "cipherText: " ++ show cipherText
    print $ "decrypted is same as text ==> " ++ show (result == text)

testBruteForceMtSeed :: IO ()
testBruteForceMtSeed = do
    seed <- randomIO
    let seed' = seed `mod` (2^16)
        cipher = getCipher (mkStdGen seed') seed'
        guess = bruteForceMtSeed cipher
    print $ "guess:" ++ show guess ++ ", seed:" ++ show seed'

testIsMtTimeSeedToken :: IO ()
testIsMtTimeSeedToken = do
    token <- generateResetToken
    shouldBeTrue <- isMtTimeSeededToken token
    shouldBeFalse <- isMtTimeSeededToken $ BC.replicate 16 'A'
    print $ "should be True ==> " ++ show shouldBeTrue
    print $ "should be False ==> " ++ show shouldBeFalse

main :: IO ()
main = do
    testStreamCipher
    testBruteForceMtSeed
    testIsMtTimeSeedToken
