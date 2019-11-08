import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import qualified Lib

data UserProfile = UserProfile (Map.Map String String) deriving Show

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
    return $ UserProfile $ Map.fromList kvs

urlParse :: String -> UserProfile
urlParse url = extractS profile url

-- urlUnparse :: UserProfile -> String
-- urlUnparse = 
    
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
profileFor email = UserProfile 
                 $ Map.fromList [ ("email", clean)
                                , ("role", "user")
                                , ("uid", "10")
                                ]
    where clean = cleanEmail email

-- Tests --

testUrlParser :: IO ()
testUrlParser = do
    let exampleUrl = "foo=bar&baz=qux&zap=zazzle"
    putStrLn $ show $ extractS profile exampleUrl

testCleanEmail :: IO ()
testCleanEmail = do
    print $ cleanEmail "foo@bar.com&role=admin"

testProfileFor :: IO ()
testProfileFor = do
    print $ profileFor "foo@bar.com&role=admin"

main:: IO ()
main = do
    testUrlParser
    testCleanEmail
    testProfileFor
