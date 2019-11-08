import Text.ParserCombinators.ReadP
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char(ord)
import Data.List(intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word(Word8)
import Debug.Trace
import System.Environment

import qualified Lib

type UserProfile = Map.Map String String

extractS :: ReadP a -> String -> a
-- extractS p s = head [x | (x,"") <- readP_to_S p s]
extractS p s = fst . last $ readP_to_S p s

lettersAndNumbers :: Set.Set Char
lettersAndNumbers = Set.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- URL Parsing utilities --
urlChars :: Set.Set Char
urlChars = lettersAndNumbers `Set.union` (Set.fromList "@%._")

pAmp :: ReadP Char
pAmp = satisfy ('&' ==)

pEquals :: ReadP Char
pEquals = satisfy ('=' ==)


pUrlChar :: ReadP Char
pUrlChar = satisfy (\c -> c `Set.member` urlChars)


pKey :: ReadP String
pKey = do
    key <- many1 pUrlChar
    pEquals
    return key
    

pValue :: ReadP String
pValue = do
    value <- many1 pUrlChar
    optional pAmp
    return value

pKV :: ReadP (String, String)
pKV = do
    key <- pKey
    value <- pValue
    return (key, value)

profile :: ReadP UserProfile
profile = do
    kvs <- many1 pKV
    return $ Map.fromList kvs

urlParse :: String -> UserProfile
urlParse url = extractS profile url

urlUnparse :: UserProfile -> String
urlUnparse profile = intercalate "&" 
                   $ Map.foldlWithKey foldFunc [] profile
    where
        foldFunc = (\a k v -> (k ++ "=" ++ v):a)
    
-- Email Parsing Utils --
emailChars :: Set.Set Char
emailChars = lettersAndNumbers `Set.union` (Set.fromList "@._")

pEmailChar :: ReadP Char
pEmailChar = satisfy (`Set.member` emailChars)

pEmail :: ReadP String
pEmail = do
    validEmail <- many1 pEmailChar
    return $ validEmail

cleanEmail :: String -> String
cleanEmail email = extractS pEmail email

-- Oracle Utilities --
profileFor :: String -> UserProfile
profileFor email = Map.fromList [ ("email", clean)
                                , ("role", "user")
                                , ("uid", "10")
                                ]
    where clean = cleanEmail email

aesKey :: B.ByteString
aesKey = Lib.hexStringToBytes "da5424c65581ec4746423e2a9b2f09c7"

encodedProfile :: String -> B.ByteString
encodedProfile email = Lib.bytesToHex rawEncoded
    where 
        aes = Lib.initAES128 aesKey
        profileString = Lib.stringToBytes . urlUnparse $ profileFor email
        rawEncoded = Lib.ecbEncryption aes profileString

decodedProfileString :: B.ByteString -> String
decodedProfileString rawEncoded = profileString
    where
        aes = Lib.initAES128 aesKey
        encoded = Lib.hexToBytes rawEncoded
        profileBytes= Lib.ecbDecryption aes encoded
        profileString = Lib.bytesToString profileBytes
        
    
decodedProfile :: B.ByteString -> UserProfile
decodedProfile rawEncoded = urlParse $ decodedProfileString rawEncoded

-- Attack --
newBlockPadddingLength :: Int
newBlockPadddingLength = 9

stdPadding :: String
stdPadding = replicate newBlockPadddingLength 'A'

encodingForWordAtOffset :: String -> Int -> B.ByteString
encodingForWordAtOffset word offset = result
    where
        padding = stdPadding ++ (replicate offset 'A')
        payload = padding ++ word
        cipherText = encodedProfile payload
        chunks = Lib.splitIntoChunks 32  cipherText
        chunk = chunks !! 2
        result = B.take (2*length word) . B.drop (2*offset) $ chunk

hexify :: Word8 -> B.ByteString
hexify l = Lib.bytesToHex $ B.singleton l

role1 :: B.ByteString
role1 = encodingForWordAtOffset "role" 1

admin6 :: B.ByteString
admin6 = encodingForWordAtOffset "admin" 6

one12 :: B.ByteString 
one12 = encodingForWordAtOffset "A" 12

two14 :: B.ByteString
two14 = encodingForWordAtOffset "AA" 14

fillCandidate :: Word8 -> Word8 -> B.ByteString
fillCandidate a b = stdCipherText `B.append` payload
    where
        payload = hexA 
                `B.append` role1 
                `B.append` hexB 
                `B.append` admin6
                `B.append` garbage
        garbage = hexA `B.append` one12 `B.append` hexB `B.append` two14
        hexA = hexify a
        hexB = hexify b
        stdCipherText =  encodedProfile stdPadding

candidates :: [B.ByteString]
candidates = (map fillCandidate [0..255]) <*> [0..255]

tryCandidate :: B.ByteString -> Bool
tryCandidate c = hasKeyA || roleIsMember && roleIsAdmin 
    where
        profile = decodedProfile c
        hasKeyA = "A" `Map.member` profile
        roleIsMember = "role" `Map.member` profile
        roleIsAdmin = (==) "admin" $ profile Map.! "role"

tryCandidates :: [(B.ByteString, Bool)]
tryCandidates = filter snd results
    where 
        results = Lib.mapWithOrig tryCandidate candidates

-- Tests --

testUrlParser :: IO ()
testUrlParser = do
    let exampleUrl = "foo=bar&baz=qux&zap=zazzle&baz=snd"
    putStrLn $ show $ extractS profile exampleUrl

testCleanEmail :: IO ()
testCleanEmail = do
    print $ cleanEmail "foo@bar.com&role=admin"

testProfileFor :: IO ()
testProfileFor = do
    print $ profileFor "foo@bar.com&role=admin"

testUrlUnparse :: IO ()
testUrlUnparse = do
    print $ urlUnparse $ profileFor "foo@bar.com&role=admin"

testEncodingAndDecoding :: IO ()
testEncodingAndDecoding = do
    putStr "This should be true ==>"
    print $ (decodedProfile $ encodedProfile "shart@gmail.com") Map.! "email"

testEncodingForWordAtOffset :: IO ()
testEncodingForWordAtOffset = do
    putStr "This should be '6ed43f5e' ==>"
    print $ encodingForWordAtOffset "role" 0

    putStr "This should be 'bcde7e3e' ==>"
    print $ encodingForWordAtOffset "role" 3

testTryCandidates :: IO ()
testTryCandidates = do
    print $ tryCandidates

runTests :: IO ()
runTests = do
    testUrlParser
    testCleanEmail
    testProfileFor
    testUrlUnparse
    testEncodingAndDecoding
    testEncodingForWordAtOffset
    testTryCandidates

runEncode :: String -> IO ()
runEncode email = do
    print $ encodedProfile email

runAttempt :: String -> IO ()
runAttempt encoded = do
    let rawEncoded = Lib.stringToBytes encoded
        profile = decodedProfile rawEncoded
    print profile
    print $ profile Map.! "role"

main:: IO ()
main = do
    args <- getArgs
    if length args == 0
        then runTests
        else case args !! 0 of "email" -> runEncode $ args !! 1
                               "attempt" -> runAttempt $ args !! 1
       
