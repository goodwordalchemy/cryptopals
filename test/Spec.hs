import Test.HUnit

import qualified Challenge1 as C1
import qualified Challenge2 as C2
import qualified Challenge3 as C3
import qualified Challenge4 as C4
import qualified Challenge5 as C5
import qualified Challenge6 as C6


challenge1Test = TestCase 
               $ assertEqual 
                    "Challenge 1: Convert hex to base64" 
                    C1.challenge1
                    C1.base64Output 

challenge2Test = TestCase 
               $ assertEqual 
                    "Challenge 2: Fixed XOR"
                    C2.challenge2 
                    C2.expectedCipherText

challenge3Test = TestCase
                $ assertEqual "Single-byte XOR cipher" C3.challenge3 'X'

challenge4Test = TestCase 
               $ do 
                   c4Result <- C4.challenge4
                   assertEqual "Detect single-character XOR" c4Result '5'
                    
challenge5Test = TestCase 
               $ assertEqual 
                    "Challenge 5: Implement repeating-key XOR"
                    C5.challenge5
                    C5.expectedCipherText

chalenge6Test = TestCase
              $ do 
                  result <- C6.challenge6
                  assertEqual
                      "Challenge 6: Break repeating-key XOR"
                      result
                      ["I'm", "Back"]
                    

testList = TestList [ challenge1Test
                    , challenge2Test
                    , challenge3Test
                    , challenge4Test
                    ]
main :: IO ()
main = do
    runTestTT testList
    return ()
