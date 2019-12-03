-- copied largely from https://github.com/emillon/cryptopals/blob/bc7b3930722af532c83e44ae2a5271d2d88a7e6f/Digest.hs
module MD4 ( md4
           , md4FromState
           , md4Prepare
           , md4Tests
           , md4PrefixMac
           , checkMd4PrefixMac
           ) where

import Data.Array
import Data.Bits
import Data.List
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Test.HUnit hiding (State)
import Test.QuickCheck hiding ((.&.))

import qualified Lib

string2bs = BC.pack

w32LEtoBS = Lib.littleEndian32 . fromIntegral

-- | Fetch the nth 32-bit word in a bytestring, in little endian order.
bsToW32BE :: B.ByteString -> Word32
bsToW32BE bs =
    sum [ 0x0000001 * get 3
        , 0x0000100 * get 2
        , 0x0010000 * get 1
        , 0x1000000 * get 0
        ]
        where
            get i = fromIntegral $ B.index bs i

bsToW32LE :: B.ByteString -> Word32
bsToW32LE =
    bsToW32BE . B.reverse

bsToNthW32LE :: B.ByteString -> Int -> Word32
bsToNthW32LE bs n =
    let bl = B.length bs in
    if bl < 4*(n+1) then
        error $ "bsToNthW32LE(n=" ++ show n ++ ") : bytestring has length " ++ show bl
    else
        bsToW32LE $ B.take 4 $ B.drop (4*n) bs

-- | Split a 64-bit into two 32-bit words.
splitW64 :: Word64
         -> (Word32, Word32) -- ^ (lo, hi)
splitW64 w =
    (lo, hi)
        where
            lo = fromIntegral $ (w .&. 0xffffffff)
            hi = fromIntegral $ (w .&. 0xffffffff00000000) `shiftR` 32

-- | HUnit tests for the MD4 implementation.
md4Tests :: Test
md4Tests =
    "MD4" ~: map (uncurry tc)
        [ (""
          , "31d6cfe0d16ae931b73c59d7e0c089c0"
          )
        , ( "a"
          , "bde52cb31de33e46245e05fbdbd6fb24"
          )
        , ( "abc"
          , "a448017aaf21d8525fc10ae87aa6729d"
          )
        , ( "message digest"
          , "d9130a8164549fe818874806e1c7014b"
          )
        , ( "abcdefghijklmnopqrstuvwxyz"
          , "d79e1c308aa5bbcdeea8ed63df412da9"
          )
        , ( "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
          , "043f8582f241db351ce627e153e7f0e4"
          )
        , ( "12345678901234567890123456789012345678901234567890123456789012345678901234567890"
          , "e33b4ddc9c38f2199c3e7b164fcc0536"
          )
        ]
    where
        tc input spec =
            Lib.hexStringToBytes spec ~=? md4 (string2bs input)

-- | Compute the MD4 digest of a message (not hex-encoded!)
md4 :: B.ByteString -> B.ByteString
md4 bs =
    md4digest $ foldl' md4update md4initState $ Lib.splitIntoChunks (16*4) $ md4Prepare bs

md4FromState 
    :: (Word32, Word32, Word32, Word32) -> B.ByteString -> B.ByteString
md4FromState (a,b,c,d) unpadded_bs =
    md4digest $ foldl' md4update md4interState $ Lib.splitIntoChunks (16*4) $ unpadded_bs
    where
        md4interState = MD4S a b c d


md4Prepare :: B.ByteString -> B.ByteString
md4Prepare bs =
    B.append bs $ md4Padding bs

md4Padding :: B.ByteString -> B.ByteString
md4Padding = md4PaddingLen . B.length

md4PaddingLen :: Int -> B.ByteString
md4PaddingLen n =
    B.concat [B.singleton 0x80, pad, sizelo, sizehi]
        where
            modsize = (n+1) `mod` 64
            npad = (56 - modsize) `mod` 64
            pad = B.replicate npad 0x00
            sizelo = w32LEtoBS lo
            sizehi = w32LEtoBS hi
            (lo, hi) = splitW64 $ fromIntegral $ 8 * n

data MD4State = MD4S { md4sA :: !Word32
                     , md4sB :: !Word32
                     , md4sC :: !Word32
                     , md4sD :: !Word32
                     }
    deriving (Eq, Show)

instance Arbitrary MD4State where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ MD4S a b c d

md4initState :: MD4State
md4initState = MD4S 0x67452301 0xefcdab89 0x98badcfe 0x10325476

md4digest :: MD4State -> B.ByteString
md4digest (MD4S a b c d) =
    B.concat $ map w32LEtoBS [a, b, c, d]

md4F, md4G, md4H :: Word32 -> Word32 -> Word32 -> Word32
md4F x y z = (x .&. y) .|. (complement x .&. z)
md4G x y z = (x .&. y) .|. (x .&. z) .|. (y .&. z)
md4H x y z = x `xor` y `xor` z

md4StateAdd :: MD4State -> MD4State -> MD4State
md4StateAdd (MD4S xa xb xc xd) (MD4S ya yb yc yd) =
    MD4S (xa + ya) (xb + yb) (xc + yc) (xd + yd)

md4update :: MD4State -> B.ByteString -> MD4State
md4update s0 bs =
    md4StateAdd s0 s'
        where
            s' = md4rounds x s0
            x = listArray (0, 15) $ map (bsToNthW32LE bs) [0..15]

type Lens r a = (r -> a, a -> r -> r)

lensA, lensB, lensC, lensD :: Lens MD4State Word32
lensA = (md4sA, \ x r -> r { md4sA = x })
lensB = (md4sB, \ x r -> r { md4sB = x })
lensC = (md4sC, \ x r -> r { md4sC = x })
lensD = (md4sD, \ x r -> r { md4sD = x })

type Lens4 = (Lens MD4State Word32, Lens MD4State Word32, Lens MD4State Word32, Lens MD4State Word32)

abcd, dabc, cdab, bcda :: Lens4
abcd = (lensA, lensB, lensC, lensD)
dabc = (lensD, lensA, lensB, lensC)
cdab = (lensC, lensD, lensA, lensB)
bcda = (lensB, lensC, lensD, lensA)

execRounds :: [(Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32,
                Lens4, Int, Int)]
              -> MD4State -> MD4State
execRounds rs s0 = foldl' go s0 rs
    where
        go st (f, ((ga, sa), (gb, _), (gc, _), (gd, _)), xk, s) =
            sa (f xk s (ga st) (gb st) (gc st) (gd st)) st

md4rounds :: Array Int Word32 -> MD4State -> MD4State
md4rounds x = execRounds
    [ (f1, abcd,  0,  3), (f1, dabc,  1,  7), (f1, cdab,  2, 11), (f1, bcda,  3, 19)
    , (f1, abcd,  4,  3), (f1, dabc,  5,  7), (f1, cdab,  6, 11), (f1, bcda,  7, 19)
    , (f1, abcd,  8,  3), (f1, dabc,  9,  7), (f1, cdab, 10, 11), (f1, bcda, 11, 19)
    , (f1, abcd, 12,  3), (f1, dabc, 13,  7), (f1, cdab, 14, 11), (f1, bcda, 15, 19)
    , (f2, abcd,  0,  3), (f2, dabc,  4,  5), (f2, cdab,  8,  9), (f2, bcda, 12, 13)
    , (f2, abcd,  1,  3), (f2, dabc,  5,  5), (f2, cdab,  9,  9), (f2, bcda, 13, 13)
    , (f2, abcd,  2,  3), (f2, dabc,  6,  5), (f2, cdab, 10,  9), (f2, bcda, 14, 13)
    , (f2, abcd,  3,  3), (f2, dabc,  7,  5), (f2, cdab, 11,  9), (f2, bcda, 15, 13)
    , (f3, abcd,  0,  3), (f3, dabc,  8,  9), (f3, cdab,  4, 11), (f3, bcda, 12, 15)
    , (f3, abcd,  2,  3), (f3, dabc, 10,  9), (f3, cdab,  6, 11), (f3, bcda, 14, 15)
    , (f3, abcd,  1,  3), (f3, dabc,  9,  9), (f3, cdab,  5, 11), (f3, bcda, 13, 15)
    , (f3, abcd,  3,  3), (f3, dabc, 11,  9), (f3, cdab,  7, 11), (f3, bcda, 15, 15)
    ]
        where
            f1 k s a b c d = (a + md4F b c d + (x!k)) `rotateL` s
            f2 k s a b c d = (a + md4G b c d + (x!k) + 0x5a827999) `rotateL` s
            f3 k s a b c d = (a + md4H b c d + (x!k) + 0x6ed9eba1) `rotateL` s

-- | Compute the MD4 prefix-MAC of a message under a given key.
md4PrefixMac :: B.ByteString -- ^ Key
             -> B.ByteString -- ^ Message
             -> B.ByteString
md4PrefixMac key message =
    md4 $ B.append key message

-- | Check the validity of a MD4 prefix-MAC.
checkMd4PrefixMac :: B.ByteString -- ^ Key
                  -> B.ByteString -- ^ Message
                  -> B.ByteString -- ^ MAC
                  -> Bool
checkMd4PrefixMac key message mac =
    md4PrefixMac key message == mac
