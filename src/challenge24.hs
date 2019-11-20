import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random

import qualified Lib
import MersenneTwister( cloneMt
                      , getMtInts
                      , MTState
                      , seedMt )

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

type MTOracle = B.ByteString -> B.ByteString

randomBS :: Int -> Int -> Int -> B.ByteString
randomBS seed lowLength highLength = result
    where result = B.pack
                 $ take l 
                 $ randomRs (0, 255) (mkStdGen seed+1)

getOracle :: Int -> MTOracle
getOracle seed text = mtEncrypt seed extended
    where 
        extended = 
        nBytes = randomRs (10, 100)

bruteForceMtSeedHelper :: MTOracle -> Int -> Int -> Int
bruteForceMtSeedHelper oracle cur stop 
  | cur == stop = error "could not find seed"
  | output = knownText = cur
  | otherwise = bruteForceMtSeedHelper cipher (cur+1) stop
    where
        output = 
        keyStream = 
        
        knownText = BC.pack "12345678901234"


bruteForceMtSeed :: MTCipher -> Int
bruteForceMtSeed cipher = seed

-- Tests
testStreamCipher :: IO ()
testStreamCipher = do
    let text = BC.pack "My shoes are awefully shiney"
        cipherText = mtEncrypt 42 text
        result = mtEncrypt 42 cipherText
    print $ "cipherText: " ++ show cipherText
    print $ "decrypted is same as text ==> " ++ show (result == text)

main :: IO ()
main = do
    testStreamCipher
