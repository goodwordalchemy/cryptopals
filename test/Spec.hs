import Test.HUnit

import qualified Challenge1 as C1
import qualified Challenge2 as C2
import qualified Challenge3 as C3


challenge1Test = TestCase 
               $ assertEqual 
                    "Challenge 1: Convert hex to base64" 
                    C1.challenge1
                    C1.base64Output 
    where
        hexInput :: String
        hexInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

        base64Output :: String
        base64Output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

challenge2Test = TestCase 
               $ assertEqual 
                    "Challenge 2: Fixed XOR"
                    C2.challenge2 
                    C2.expectedCipherText

challenge3Test = TestCase
                $ assertEqual
                    "Single-byte XOR cipher"
                    C3.challenge3
                    'X'
                    

testList = TestList [ challenge1Test
                    , challenge2Test
                    , challenge3Test
                    ]
main :: IO ()
main = do
    runTestTT testList
    return ()
