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
import qualified Challenge16 as C16
import qualified Challenge17 as C17
import qualified Challenge18 as C18
import qualified Challenge19 as C19
import qualified Challenge20 as C20
import qualified Challenge21 as C21
-- testing not applicable for C22
import qualified Challenge23 as C23
import qualified Challenge24 as C24
import qualified Challenge25 as C25
import qualified Challenge26 as C26
import qualified Challenge27 as C27
import qualified Challenge28 as C28
import qualified Challenge29 as C29
import qualified Challenge30 as C30

import MD4(md4Tests)

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

challenge16Test = TestCase
                $ do
                    result <- C16.challenge16
                    assertEqual
                      "Challenge 16: BC bitflipping attacks"
                      True
                      result

challenge17Test = TestCase
                $ assertEqual
                      "Challenge 17: The CBC padding oracle"
                      True
                      C17.challenge17

challenge18Test = TestCase
                $ assertEqual
                      "Challenge 18: Implement CTR, the stream cipher mode"
                      (True, True)
                      C18.challenge18

challenge19Test = TestCase
                $ assertEqual
                      "Challenge 19: Break fixed-nonce CTR mode using substitutions"
                      True
                      C19.challenge19

challenge20Test = TestCase
                $ do
                    result <- C20.challenge20
                    assertEqual
                      "Challenge 20: Break fixed-nonce CTR statistically"
                      True
                      result

challenge21Test = TestCase
                $ assertEqual
                      "Challenge 21: Implement the MT19937 Mersenne Twister RNG"
                      True
                      C21.challenge21

challenge23Test = TestCase
                $ assertEqual
                      "Challenge 23: Clone an MT19937 RNG from its output"
                      True
                      C23.challenge23

challenge24Test = TestCase
                $ assertEqual
                      "Challenge 24: Create the MT19937 stream cipher and break it"
                      True
                      C24.challenge24

challenge25Test = TestCase
                $ assertEqual
                      "Challenge 25: Break \"random access read/write\" AES CTR"
                      True
                      C25.challenge25

challenge26Test = TestCase
                $ assertEqual
                      "Challenge 26: CTR bitflipping"
                      True
                      C26.challenge26

challenge27Test = TestCase
                $ assertEqual
                      "Challenge 27: Recover the key from CBC with IV=Key"
                      True
                      C27.challenge27

challenge28Test = TestCase
                $ assertEqual
                      "Challenge 28: Implement a SHA-1 keyed MAC"
                      True
                      C28.challenge28

challenge29Test = TestCase
                $ assertEqual
                      "Challenge 29: Break a SHA-1 keyed MAC using length extension"
                      True
                      C29.challenge29

challenge30Test = TestCase
                $ assertEqual
                      "Challenge 30: Break an MD4 keyed MAC using length extension"
                      True
                      C30.challenge30

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
                    , challenge16Test
                    , challenge17Test
                    , challenge18Test
                    , challenge19Test
                    , challenge20Test
                    , challenge21Test
                    , challenge23Test
                    , challenge24Test
                    , challenge25Test
                    , challenge26Test
                    , challenge27Test
                    , challenge28Test
                    , challenge29Test
                    , md4Tests
                    , challenge30Test
                    ]
main :: IO ()
main = do
    runTestTT testList
    return ()
