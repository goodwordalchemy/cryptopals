module DiffieHellman( p
                    , g
                    , genPublicKey
                    , getCommonKey
                    ) where

import Data.Bits(shiftR)
p :: Integer
p = 0xffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da48361c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552bb9ed529077096966d670c354e4abc9804f1746c08ca237327ffffffffffffffff

g :: Integer
g = 2
modexp :: Integer -> Integer -> Integer -> Integer
modexp base exponent modulus 
  | modulus == 1 = 0
  | otherwise = go (base `mod` modulus) exponent 1
    where 
        go b e result
          | e == 0 = result
          | otherwise = go b' e' result'
            where
                result' = 
                    if e' `mod` 2 == 1 
                       then (result * b) `mod` modulus
                       else result
                e' = e `shiftR` 1
                b' = (b*b) `mod` modulus

genPublicKey :: Integer -> Integer -> Integer -> Integer
genPublicKey base privateKey modulus = 
    modexp base privateKey modulus

getCommonKey :: Integer -> Integer -> Integer -> Integer
getCommonKey yourPublic myPrivate modulus = 
    modexp yourPublic myPrivate modulus
