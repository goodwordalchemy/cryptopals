module Challenge16(challenge16) where

import Crypto.Cipher(AES128)
import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(chr, ord)
import Data.Word(Word8)
import Debug.Trace
import System.Random(newStdGen)

import qualified Lib

nthBlock16 :: Int -> B.ByteString -> B.ByteString
nthBlock16 n text = (Lib.chunks16 text) !! n

-- Oracle Setup --
data CipherDirection = Encryption | Decryption
type Device = CipherDirection -> B.ByteString -> B.ByteString

getCBCEncryptionDevice :: IO Device
getCBCEncryptionDevice = do
    gen <- newStdGen
    let (key, gen') = Lib.getRandomAESKey gen
        (iv, _) = Lib.getRandomAESKey gen
        aes = Lib.initAES128 key
        func = (\direction input -> 
            case direction of 
              Encryption -> Lib.cbcEncryption aes iv input
              Decryption -> Lib.cbcDecryption aes iv input)
    return func

prefix :: B.ByteString
prefix = BC.pack "comment1=cooking%20MCs;userdata="

suffix :: B.ByteString
suffix = BC.pack ";comment2=%20like%20a%20pound%20of%20bacon"

concattedUserInput :: String -> B.ByteString
concattedUserInput userIn = fullIn
    where
        cleanFunc = (\c -> if c `elem` ";=" then '?' else c) 
        cleanIn = BC.map cleanFunc (BC.pack userIn)
        fullIn = B.concat [prefix, cleanIn, suffix]

encryptedUserInput :: Device -> String  -> B.ByteString
encryptedUserInput device userIn = encryptedIn
    where
        fullIn = concattedUserInput userIn
        encryptedIn = device Encryption fullIn

oracle :: Device -> B.ByteString -> Bool
oracle device encryptedIn = hasAdminString
    where
        decryptedIn = device Decryption encryptedIn
        targetString = BC.pack ";admin=true;" 
        hasAdminString = Lib.isSubstring targetString decryptedIn

-- Attack --
fillerChar :: Char
fillerChar = 'A'

nBytePayload :: Int -> B.ByteString
nBytePayload n = BC.replicate n fillerChar

xorChars :: Char -> Char -> Word8
xorChars a b = fromIntegral (ord a `xor` ord b)

replaceAtIndex :: Int -> Word8 -> B.ByteString -> B.ByteString
replaceAtIndex idx c text = (before `B.snoc` c) `B.append` rest
    where
        rest = B.tail at
        (before, at) = B.splitAt idx text

replaceAtIndices :: [(Int, Word8)] -> B.ByteString -> B.ByteString
replaceAtIndices [] text = text
replaceAtIndices ((idx, char):ics) text = replaceAtIndices ics newText
    where newText = replaceAtIndex idx char text

aaas :: String
aaas = replicate 16 'A'

replacementRequests :: [(Int, Char)]
replacementRequests = [ (0, ';')
                      , (6, '=')
                      , (11, ';')
                      , (13, '=')
                      ]

getReplacements :: B.ByteString -> [(Int, Word8)]
getReplacements prevBlock = replacements
    where 
        replacements = map replacementLetter replacementRequests
        replacementLetter (idx, letter) = (idx, doXor letter idx)
        doXor letter idx = (letterAsWord8 'A') 
                     `xor` (letterAsWord8 letter) 
                     `xor` B.index prevBlock idx
        letterAsWord8 letter = ((fromIntegral $ ord letter)::Word8)

precedingBlockForAttack :: Device -> B.ByteString -> B.ByteString
precedingBlockForAttack device cipherText = attackBlock
    where
        prevBlockIdx = (B.length prefix) `div` 16
        prevBlock = nthBlock16 prevBlockIdx cipherText

        attackBlock = replaceAtIndices replacements prevBlock

        replacements = getReplacements prevBlock 
        

replaceBlock :: Int -> B.ByteString -> B.ByteString -> B.ByteString
replaceBlock n fullOrig replacement = replaced
    where
        blocks = Lib.chunks16 fullOrig
        (start, at) = splitAt n blocks
        (_, rest) = splitAt 1 at
        front = B.concat start
        back = B.concat rest
        replaced = B.concat [front, replacement, back]
        
-- NOTE: assumes length of prefix is mutliple of 16
getAttackString :: Device -> B.ByteString
getAttackString device = replaced
    where
        replaced = replaceBlock prevBlockIdx origCipherText replacement 
        prevBlockIdx = B.length prefix `div` 16
        replacement = precedingBlockForAttack device origCipherText
        origCipherText = encryptedUserInput device payload
        payload = aaas ++ "AadminAtrueAfAba"

-- Testing --
testEncryptedUserInput :: IO ()
testEncryptedUserInput = do
    print $ concattedUserInput "foo=barls;kkd"

    device <- getCBCEncryptionDevice
    print $ encryptedUserInput device "foo=barls;kkd"


testDecryptedUserInput :: IO ()
testDecryptedUserInput = do
    device <- getCBCEncryptionDevice
    let userInput = aaas ++ "AadminAtrueAfAba" 
        encrypted = encryptedUserInput device userInput
        decrypted = device Decryption encrypted
    print $ "Decrypted: " ++ show decrypted

challenge16 :: IO Bool
challenge16 = do
    device <- getCBCEncryptionDevice
    let attackString = getAttackString device
        result = oracle device attackString
    return result

testAttack :: IO ()
testAttack = do
    result <- challenge16
    print $ "oracle gives: " ++ show result
    
main :: IO ()
main = do
    -- testDecryptedUserInput
    testAttack
