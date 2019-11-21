module Challenge26(challenge26) where
import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(ord)
import Data.Word(Word8)
import Debug.Trace

import qualified Lib

type Device = B.ByteString -> B.ByteString

-- Attack
blockIdx :: Int
blockIdx = (B.length prefix) `div` 16

nthBlock16 :: Int -> B.ByteString -> B.ByteString
nthBlock16 n text = (Lib.chunks16 text) !! n

fillerChar :: Char
fillerChar = 'A'

nBytePayload :: Int -> B.ByteString
nBytePayload n = BC.replicate n fillerChar

xorChars :: Char -> Char -> Word8
xorChars a b = fromIntegral (ord a `xor` ord b)


aaas :: String
aaas = replicate 16 'A'

replacementRequests :: [(Int, Char)]
replacementRequests = [ (0, ';')
                      , (6, '=')
                      , (11, ';')
                      , (13, '=')
                      ]

getReplacements :: B.ByteString -> [(Int, Word8)]
getReplacements block = replacements
    where 
        replacements = map replacementLetter replacementRequests
        replacementLetter (idx, letter) = (idx, doXor letter idx)
        doXor letter idx = (letterAsWord8 'A')
                     `xor` (letterAsWord8 letter) 
                     `xor` B.index block idx
        letterAsWord8 letter = ((fromIntegral $ ord letter)::Word8)

getReplacementBlock :: B.ByteString -> B.ByteString
getReplacementBlock cipherText = attackBlock
    where
        attackBlock = Lib.replaceAtIndices replacements block

        replacements = getReplacements block 

        block = nthBlock16 blockIdx cipherText


-- NOTE: assumes length of prefix is mutliple of 16
getAttackString :: Device -> B.ByteString
getAttackString device =  replaced
    where
        replaced = Lib.replaceBlock blockIdx origCipherText replacement 
        replacement = getReplacementBlock origCipherText
        origCipherText = encryptedUserInput device payload
        payload = "AadminAtrueAfAba"

-- Oracle setup

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
        encryptedIn = device fullIn

oracle :: Device -> B.ByteString -> Bool
oracle device encryptedIn = hasAdminString
    where
        decryptedIn = device encryptedIn
        targetString = BC.pack ";admin=true;" 
        hasAdminString = Lib.isSubstring targetString decryptedIn

-- Tests
ctrDevice :: Device 
ctrDevice = Lib.getCTRDevice key nonce
    where
        key = BC.replicate 16 'A' 
        nonce = 42

testDecryptedUserInput :: IO ()
testDecryptedUserInput = do
    let userInput = aaas ++ "AadminAtrueAfAba" 
        encrypted = encryptedUserInput ctrDevice userInput
        decrypted = ctrDevice encrypted
    print $ "Decrypted: " ++ show decrypted

challenge26 :: Bool
challenge26 = 
    let device = ctrDevice
        attackString = getAttackString device
        result = oracle device attackString
    in result

testCTRBitflippingAttack :: IO ()
testCTRBitflippingAttack = do
    print challenge26

main :: IO ()
main = do
    -- testDecryptedUserInput
    testCTRBitflippingAttack
