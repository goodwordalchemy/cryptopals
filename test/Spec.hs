import Test.HUnit

import qualified Challenge1 as C1
import qualified Challenge2 as C2
import qualified Challenge3 as C3
import qualified Challenge4 as C4
import qualified Challenge5 as C5
import qualified Challenge6 as C6
import qualified Challenge7 as C7
import qualified Challenge8 as C8


challenge1Test = TestCase 
               $ assertEqual 
                    "Challenge 1: Convert hex to base64" 
                    C1.base64Output 
                    C1.challenge1

challenge2Test = TestCase 
               $ assertEqual 
                    "Challenge 2: Fixed XOR"
                    C2.expectedCipherText
                    C2.challenge2 

challenge3Test = TestCase
                $ assertEqual "Single-byte XOR cipher" 'X' C3.challenge3 

challenge4Test = TestCase 
               $ do 
                   c4Result <- C4.challenge4
                   assertEqual "Detect single-character XOR" '5' c4Result 
                    
challenge5Test = TestCase 
               $ assertEqual 
                    "Challenge 5: Implement repeating-key XOR"
                    C5.expectedCipherText
                    C5.challenge5

challenge6Test = TestCase
              $ do 
                  result <- C6.challenge6
                  assertEqual
                      "Challenge 6: Break repeating-key XOR"
                      ["I'm", "back"]
                      result
                    
challenge7Test = TestCase
              $ do 
                  result <- C7.challenge7
                  assertEqual
                      "Challenge 7: AES in ECB mode"
                      ["I'm", "back"]
                      result

challenge8Test = TestCase
              $ do 
                  result <- C8.challenge8
                  assertEqual
                      "Challenge 8: Detect AES in ECB mode"
                      1
                      result

testList = TestList [ challenge1Test
                    , challenge2Test
                    , challenge3Test
                    , challenge4Test
                    , challenge5Test
                    , challenge6Test
                    , challenge7Test
                    , challenge8Test
                    ]
main :: IO ()
main = do
    runTestTT testList
    return ()
