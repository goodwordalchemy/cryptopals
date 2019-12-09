module Challenge33(challenge33) where
import Data.Bits(shiftR)
import DiffieHellman( p
                    , g
                    , genPublicKey
                    , getCommonKey
                    ) 

a :: Integer
a = p - 33

b :: Integer
b = 22

_A :: Integer
_A = genPublicKey g a p

_B :: Integer
_B = genPublicKey g b p

sA :: Integer
sA = getCommonKey _B a p

sB :: Integer
sB = getCommonKey _A b p

challenge33 :: Bool
challenge33 = sA == sB

main :: IO ()
main = do
    print $ "a, _A, sA" ++ show (a, _A, sA)
    print $ "b, _B, sB" ++ show (b, _B, sB)
    print $ sA == sB
