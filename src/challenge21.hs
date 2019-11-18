module Challenge21(challenge21) where

import Control.Monad.State
import MersenneTwister(mtInt, seedMt)

expectedSequenceForSeed0 :: [Int]
expectedSequenceForSeed0 = [ 2357136044, 2546248239, 3071714933, 3626093760,
                             2588848963, 3684848379, 2340255427, 3638918503,
                             1819583497, 2678185683
                           ]

first10GeneratedNumbers :: [Int]
first10GeneratedNumbers = evalState (sequence $ replicate 10 mtInt) (seedMt 0)

challenge21 :: Bool
challenge21 = first10GeneratedNumbers == expectedSequenceForSeed0

main :: IO ()
main = pure challenge21 >>= print
