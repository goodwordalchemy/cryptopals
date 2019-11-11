import Crypto.Cipher(AES128)
import Data.Bits(xor)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(ord)
import Data.Word(Word8)
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

xorChar :: Char -> Word8
xorChar target = fromIntegral (ord fillerChar `xor` ord target)

semicolonXorChar :: Word8
semicolonXorChar = xorChar ';'

equalsXorChar :: Word8
equalsXorChar = xorChar '='

xorNthChar :: Int -> Word8 -> B.ByteString -> B.ByteString
xorNthChar n c text = (before `B.snoc` c) `B.append` rest
    where
        rest = B.tail at
        (before, at) = B.splitAt n text

xorNthChars :: [(Int, Word8)] -> B.ByteString -> B.ByteString
xorNthChars [] text = text
xorNthChars ((idx, char):ics) text = xorNthChars ics newText
    where newText = xorNthChar idx char text

replacements :: [(Int, Word8)]
replacements = [ (0, semicolonXorChar)
               , (6, equalsXorChar)
               , (11, semicolonXorChar)
               , (13, equalsXorChar)
               ]

aaas :: String
aaas = replicate 16 'A'

precedingBlockForAttack :: Device -> B.ByteString
precedingBlockForAttack device = attackBlock
    where
        prevBlockIdx = B.length prefix `div` 16
        cipherText = encryptedUserInput device aaas
        prevBlock = nthBlock16 prevBlockIdx cipherText
        attackBlock = xorNthChars replacements prevBlock
        

replaceBlock :: Int -> B.ByteString -> B.ByteString -> B.ByteString
replaceBlock n fullOrig replacement = replaced
    where
        blocks = Lib.chunks16 fullOrig
        (start, at) = splitAt n blocks
        (_, rest) = splitAt 1 blocks
        front = B.concat start
        back = B.concat rest
        replaced = B.concat [front, replacement, back]
        
-- NOTE: assumes length of prefix is mutliple of 16
getAttackString :: Device -> B.ByteString
getAttackString device = replaced
    where
        replaced = replaceBlock prevBlockIdx replacement origCipherText
        prevBlockIdx = B.length prefix `div` 16
        replacement = precedingBlockForAttack device
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
    testEncryptedUserInput
    testAttack
