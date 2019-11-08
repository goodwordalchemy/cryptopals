import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import qualified Lib

-- URL Parsing utilities --
data UserProfile = UserProfile (Map.Map String String) deriving Show

urlChars :: [Char]
urlChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "@%"

pAmp :: ReadP Char
pAmp = satisfy ('&' ==)

pEquals :: ReadP Char
pEquals = satisfy ('=' ==)


pUrlChar :: ReadP Char
pUrlChar = satisfy (\c -> c `elem` urlChars)


pKey :: ReadP String
pKey = do
    key <- manyTill pUrlChar pEquals
    return key
    

pValue :: ReadP String
pValue = do
    value <- (manyTill pUrlChar pAmp) <++ (manyTill pUrlChar eof)
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

extractS :: ReadP a -> String -> a
extractS p s = fst . head $ readP_to_S p s
-- Tests --

testParser :: IO ()
testParser = do
    -- let exampleUrl = "foo=bar&baz=qux&zap=zazzle"
    let exampleUrl = "foo=bar"
    -- putStrLn $ show $ readP_to_S profile exampleUrl
    putStrLn $ show $ extractS profile exampleUrl

main:: IO ()
main = do
    testParser
