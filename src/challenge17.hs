import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(ord)
import Data.Word(Word8)
import System.Random(mkStdGen)

import qualified Lib

data CipherDirection = Encryption | Decryption
type Device = CipherDirection -> B.ByteString -> B.ByteString

charToWord8 :: Char -> Word8
charToWord8 c = ((fromIntegral $ ord c)::Word8)

targets :: [B.ByteString]
targets = map (Lib.base64ToBytes . BC.pack) raws
    where 
        raws = [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
               , "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
               , "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
               , "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
               , "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
               , "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
               , "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
               , "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
               , "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
               , "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
               ]

getConstant16ByteString :: Int -> B.ByteString
getConstant16ByteString seed = key 
    where 
        gen = mkStdGen seed
        (key, _) = Lib.getRandomAESKey gen



aesIv :: B.ByteString
aesIv = getConstant16ByteString 2

aesKey :: B.ByteString
aesKey = getConstant16ByteString 1

getCBC :: B.ByteString -> Device
getCBC iv = device
    where 
        aes = Lib.initAES128 aesKey

        device direction input = case direction of 
            Encryption -> Lib.cbcEncryption aes iv input
            Decryption -> Lib.cbcDecryption aes iv input

{- Note, in final implementation of this, a random number will be passed
 to this function, and I will have to maintain a mapping of ciphertexts
 to decryption states
-}

padAndEncrypt :: B.ByteString -> (B.ByteString, B.ByteString)
padAndEncrypt target = (aesIv, encrypted)
    where
        encrypted = getCBC aesIv Encryption paddedTarget
        paddedTarget = Lib.padToMultiple target 16

encryptedTarget :: (B.ByteString, B.ByteString)
encryptedTarget = padAndEncrypt target
    where
        target = targets !! 0

paddingIsValid :: B.ByteString -> B.ByteString -> Bool
paddingIsValid iv encrypted = case Lib.stripValidPadding decrypted of
        Right x -> True
        Left y -> False
    where
        decrypted = getCBC iv Decryption encrypted

-- Attack
-- paddingOracleAttack
--     :: (B.ByteString -> B.ByteString -> Bool)
--     -> B.ByteString
--     -> B.ByteString
--     -> B.ByteString
-- paddingOracleAttack getOracleFunc iv cipherText = decrypted
--     where 
--         decrypted = headBlock `B.append` (B.concat tailBlocks)
--         headBlock = decryptFirstBlock getOracleFunc iv (head chunks)
--         tailBlocks = decryptBlocks (getOracleFunc iv) chunks [1..nChunks-1]
--
--         nChunks = length chunks
--         chunks = Lib.chunks16 cipherText

decryptFirstBlock 
    :: (B.ByteString -> B.ByteString -> Bool)
    -> B.ByteString
    -> B.ByteString
    -> B.ByteString
decryptFirstBlock getOracleFunc iv cipherText = B.pack decrypted
    where
        (_, _, decrypted) = unzip3 results
        results = foldr combineSolutions [] [1..16]
        combineSolutions _ acc = (firstBlockIndexSolution 
                                        getOracleFunc
                                        acc
                                        cipherText):acc
                                        
firstBlockIndexSolution
    :: (B.ByteString -> B.ByteString -> Bool)
    -> [(Word8, Word8, Word8)]
    -> B.ByteString 
    -> (Word8, Word8, Word8)
firstBlockIndexSolution getOracleFunc soFar firstBlock = (c', i, p)
    where
        p = c `xor` i
        c = (firstBlock `B.index` (16 - 1 - knownLength))
        i = paddingChar `xor` c'
        c' = charToSatisfyPadding'iv 
                getOracleFunc
                prevBlockEnd
                firstBlock
                0

        prevBlockEnd = B.pack $ map (xor paddingChar) is
        (c's, is, ps) = unzip3 soFar
        paddingChar = (fromIntegral $ 1 + knownLength)::Word8 
        knownLength = length soFar

charToSatisfyPadding'iv 
    :: (B.ByteString -> B.ByteString -> Bool)
    -> B.ByteString
    -> B.ByteString
    -> Word8
    -> Word8
charToSatisfyPadding'iv getOracleFunc ivEnd firstBlock curChar
  | satisfied = curChar
  | curChar == 255 = error "Could not find character to satisfy oracle (iv)"
  | otherwise = charToSatisfyPadding'iv 
                    getOracleFunc 
                    ivEnd 
                    firstBlock 
                    (1+curChar)
    where
        satisfied = oracle firstBlock
        oracle = getOracleFunc iv
        iv = prefix `B.append` (curChar `B.cons` ivEnd)
        prefix = BC.replicate (16 - 1 - (B.length ivEnd)) 'A'
--
-- blockAtIndexSolution
--     :: (B.ByteString -> Bool)
--     -> [(Word8, Word8, Word8)]
--     -> Int
--     -> [B.ByteString]
--     -> (Word8, Word8, Word8) -- (C', I, P)
-- blockAtIndexSolution oracle soFar blockIdx blocks = (c', i, p)
--     where
--         p = c `xor` i
--         c = (curBlock !! (16 - 1 - knownLength))
--         i = paddingChar `xor` c'
--         c' = charToSatisfyPadding 
--                 oracle 
--                 before 
--                 prevBlockEnd 
--                 curBlock 
--                 0
--
--         curblock = blocks !! blockIdx
--         before = if blockIdx < 2 then B.empty
--                                  else B.concat 
--                                     $ fst 
--                                     $ splitAt (blockIdx-1) blocks
--         
--         prevBlockEnd = B.pack $ map (xor paddingChar) is
--         (c's, is, ps) = unzip3 soFar
--         paddingChar = (fromIntegral $ 1 + knownLength)::Word8 
--         knownLength = length soFar
--
-- charToSatisfyPadding 
--     :: (B.ByteString -> Bool)
--     -> B.ByteString
--     -> B.ByteString
--     -> B.ByteString
--     -> Word8
--     -> Word8
-- charToSatisfyPadding oracle before prevBlockEnd curBlock curChar
--   | satisfied = curChar
--   | curChar == 255 = error "Could not find character to satisfy oracle"
--   | otherwise = charToSatisfyPadding 
--                     oracle 
--                     before 
--                     prevBlockEnd 
--                     curBlock 
--                     (1+curChar)
--     where
--         satisfied = oracle (B.concat [before, prevBlock, curBlock])
--         prevBlock = prefix `B.append` (curChar `B.cons` prevBlockEnd)
--         prefix = B.replicate (16 - 1 - (B.length prevBlockEnd)) 'A'

-- Tests --

testEncryptionAndDecryption :: IO ()
testEncryptionAndDecryption = do
    let target = BC.pack "test123"
        (iv, encrypted) = padAndEncrypt target
        result = paddingIsValid iv encrypted

        target' = BC.pack "test123\09\08\09\09\09\09\09\09\09"
        (iv', encrypted') = padAndEncrypt target'
        result' = paddingIsValid iv encrypted'
    print $ (result, result')

testDecryptFirstBlock :: IO ()
testDecryptFirstBlock = do
    let (testIv, testCipher) = padAndEncrypt (BC.pack "test")
        result = decryptFirstBlock paddingIsValid testIv testCipher
    print $ "This should say 'test' ==> " ++ show result

main :: IO ()
main = do
    -- testEncryptionAndDecryption
    testDecryptFirstBlock
