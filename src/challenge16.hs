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
        decryptedIn = traceShowId $ device Decryption encryptedIn
        targetString = BC.pack ";admin=true;" 
        hasAdminString = Lib.isSubstring targetString decryptedIn

-- Attack --
fillerChar :: Char
fillerChar = 'A'

nBytePayload :: Int -> B.ByteString
nBytePayload n = BC.replicate n fillerChar

xorChars :: Char -> Char -> Word8
xorChars a b = fromIntegral (ord a `xor` ord b)

xorNthChar :: Int -> Word8 -> B.ByteString -> B.ByteString
xorNthChar n c text = (before `B.snoc` c) `B.append` rest
    where
        rest = B.tail at
        (before, at) = B.splitAt n text

replacementRequests :: [(Int, Char)]
replacementRequests = [ (0, ';')
                      , (6, '=')
                      , (11, ';')
                      , (13, '=')
                      ]

getReplacements :: B.ByteString -> [(Int, Word8)]
getReplacements text = replacements
    where 
        replacements = map replacementLetter replacementRequests
        replacementLetter (idx, letter) = (idx, doXor idx letter)
        doXor idx letter = (letterAsWord8 'A') `xor` (letterAsWord8 letter)
        -- doXor idx letter = (text `B.index` idx) `xor` (letterAsWord8 letter)
        letterAsWord8 letter = ((fromIntegral $ ord letter)::Word8)

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

precedingBlockForAttack :: Device -> B.ByteString -> B.ByteString
precedingBlockForAttack device cipherText = attackBlock
    where
        prevBlockIdx = B.length prefix `div` 16
        prevBlock = (trace $ "prevBlock:" ++ show (nthBlock16 prevBlockIdx cipherText))$ nthBlock16 prevBlockIdx cipherText
        curBlock = (trace $ "curBlock:" ++ show (nthBlock16 (prevBlockIdx+1) cipherText)) $ nthBlock16 (prevBlockIdx+1) cipherText
        replacements = getReplacements curBlock
        attackBlock = (trace $ "attackBlock:" ++ show (replaceAtIndices replacements prevBlock)) $ replaceAtIndices replacements prevBlock
        

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

testAttack :: IO ()
testAttack = do
    device <- getCBCEncryptionDevice
    let attackString = getAttackString device
        result = oracle device attackString
    print $ "oracle gives: " ++ show result
    
main :: IO ()
main = do
    -- testEncryptedUserInput
    testAttack
