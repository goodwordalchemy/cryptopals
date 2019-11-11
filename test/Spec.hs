import Test.HUnit

import qualified Challenge1 as C1
import qualified Challenge2 as C2
import qualified Challenge3 as C3
import qualified Challenge4 as C4
import qualified Challenge5 as C5
import qualified Challenge6 as C6
import qualified Challenge7 as C7
import qualified Challenge8 as C8
import qualified Challenge9 as C9
import qualified Challenge10 as C10
import qualified Challenge11 as C11
import qualified Challenge12 as C12
import qualified Challenge13 as C13
import qualified Challenge14 as C14
import qualified Challenge15 as C15


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

challenge9Test = TestCase 
               $ assertEqual 
                    "Challenge 9: Implement PKCS#7 padding"
                    [20,30,4]
                    C9.challenge9

challenge10Test = TestCase
              $ do 
                  result <- C10.challenge10
                  assertEqual
                      "Challenge 10: Implement CBC mode"
                      ["I'm", "back"]
                      result

challenge11Test = TestCase
              $ do 
                  result <- C11.challenge11
                  assertEqual
                      "Challenge 11: An ECB/CBC detection oracle"
                      100
                      result

challenge12Test = TestCase
              $ do 
                  result <- C12.challenge12
                  assertEqual
                      "Challenge 12: Byte-at-a-time ECB decryption (Simple)"
                      ["Rollin'", "in"]
                      result

-- NOTE: uuid was supposed to come before role, but in my implementation
-- it comes after, which means I can't decode a uuid, because
-- the padding characters can't be urlEncoded.  But I get the idea here.
challenge13Test = TestCase
                $ assertEqual
                      "Challenge 13: ECB cut-and-paste"
                      True
                      C13.challenge13

challenge14Test = TestCase
              $ do 
                  result <- C14.challenge14
                  assertEqual
                      "Challenge 14: Byte-at-a-time ECB decryption (Harder)"
                      (True, ["Rollin'", "in"])
                      result

challenge15Test = TestCase
                $ assertEqual
                      "Challenge 15: PKCS#7 padding validation"
                      ("ICE ICE BABY",2)
                      C15.challenge15

testList = TestList [ challenge1Test
                    , challenge2Test
                    , challenge3Test
                    , challenge4Test
                    , challenge5Test
                    , challenge6Test
                    , challenge7Test
                    , challenge8Test
                    , challenge9Test
                    , challenge10Test
                    , challenge11Test
                    , challenge12Test
                    , challenge13Test
                    , challenge14Test
                    , challenge15Test
                    ]
main :: IO ()
main = do
    runTestTT testList
    return ()
