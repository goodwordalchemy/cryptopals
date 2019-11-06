import Test.HUnit

import Challenge1(challenge1)


challenge1Test = TestCase 
               $ assertEqual 
                    "Challenge 1: Convert hex to base64" 
                    (challenge1 hexInput) 
                    base64Output 
    where
        hexInput :: String
        hexInput = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

        base64Output :: String
        base64Output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

testList = TestList [TestLabel "Challenge 1" challenge1Test
                    ]
main :: IO ()
main = do
    runTestTT testList
    return ()
