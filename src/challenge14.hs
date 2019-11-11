import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Word(Word8)
import System.Random

import BlockOracle
import qualified Lib

type IOOracle = B.ByteString -> IO B.ByteString

nBytePayload :: Int -> B.ByteString
nBytePayload n = BC.replicate n 'A'

areaICareAbout :: B.ByteString -> B.ByteString
areaICareAbout oracleOut = snd $ B.splitAt repIndex oracleOut
    where repIndex = case Lib.findRepetitionIndex oracleOut of
              Nothing -> error "could not find area with repeated substring in cipher text"
              Just idx -> idx

byteThatMakesMatchingPayload :: IOOracle -> B.ByteString -> B.ByteString -> Int -> Word8 -> Word8
byteThatMakesMatchingPayload oracle payload soFar blockNum guessByte = 
    if guess == toMatch
        then guessByte
        else if guessByte == 255 
            then error $ "didn't find a matching byte for payloadChunk:" ++ show toMatch
            else byteThatMakesMatchingPayload oracle payload soFar blockNum nextByte

    where
        nextByte = guessByte + 1
        toMatchOracleOut = oracle payload
        toMatch = blockICareAbout toMatchOracleOut
        guessOracleOut = oracle $ payload `B.append` soFar `B.snoc` guessByte
        guess = blockICareAbout $ areaICareAbout guessOracleOut

        blockICareAbout :: B.ByteString -> B.ByteString
        blockICareAbout oracleOut = (Lib.chunks16 oracleOut) !! blockNum


decryptNextByte :: IOOracle -> B.ByteString -> Word8
decryptNextByte oracle soFar = byte
    where 
        soFarLength = B.length soFar
        blockNum = soFarLength `div` 16
        padLength = 15 - (soFarLength `mod` 16)
        payload = nBytePayload padLength 
        byte = byteThatMakesMatchingPayload oracle payload soFar blockNum 0
        

byteAtATimeHelper :: IOOracle -> B.ByteString -> Int-> IO B.ByteString
byteAtATimeHelper oracle decryptedSoFar stopLength =
    if B.length decrypted >= stopLength 
       then decrypted
       else byteAtATimeHelper oracle decrypted stopLength
    where
        decrypted = decryptedSoFar `B.snoc` nextByte
        nextByte = decryptNextByte oracle decryptedSoFar


byteAtATime :: IOOracle -> IO B.ByteString
byteAtATime oracle = byteAtATimeHelper oracle B.empty lengthOfUnknown
    where 
        lengthOfUnknown = B.length unknown

getOracle :: IO IOOracle
getOracle = do
    gen <- getStdGen
    let (key, gen') = getRandomAESKey gen
        (prefix, _) = getRandomPrefix gen'
        oracle = getRandomPrefixOracle key prefix
    return oracle

testDecryptUnknown :: IO ()
testDecryptUnknown = do
    oracle <- getOracle
    result <- byteAtATime oracle
    putStrLn "================"
    putStrLn "Testing decryption"
    putStr "What is this? =>"
    print result

    
main :: IO ()
main = do
    testDecryptUnknown
