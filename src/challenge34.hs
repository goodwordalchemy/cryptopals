{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Lens
import Control.Monad.State
import Data.ByteString as B
import Data.Maybe(fromJust)
import Debug.Trace

import DiffieHellman( genPublicKey
                    , getCommonKey
                    ) 
import qualified Lib

data Person  =  Person
                { _name :: String
                , _keyChain :: KeyChain
                , _inbound :: Message
                , _outbound :: Message
    
                } deriving (Show)

type PP = (Person, Person)

data KeyChain = KeyChain
                { _privateKey :: Integer
                , _publicKey :: Maybe Integer
                , _base :: Maybe Integer
                , _modulus :: Maybe Integer
                , _commonKey :: Maybe Integer
                } deriving (Show)

data Message = Invitation 
                { _publicKey :: Integer
                , _base :: Integer
                , _modulus' :: Integer
                } 
             | Acceptance
                { _publicKey :: Integer }
             | Info
                { _content :: B.ByteString
                , _iv :: B.ByteString
                } 
             | Quiet
               deriving (Show)

$(makeLenses ''Person)
$(makeLenses ''KeyChain)
$(makeLenses ''Message)

makePerson :: String -> Integer -> Person
makePerson name privateKey = 
    let kc = KeyChain 
                { _privateKey = privateKey
                , _publicKey = Nothing
                , _modulus = Nothing
                , _commonKey = Nothing
                }
    in Person 
        { _name = name
        , _keyChain = kc
        , _inbound = Quiet
        , _outbound = Quiet
        }

updateWithBaseAndModulus :: Integer -> Integer -> Person -> Person
updateWithBaseAndModulus base_ modulus_ person =
    let priv = view (keyChain.privateKey) person
        pub = genPublicKey base_ priv modulus_
        
        person' = set (keyChain.publicKey) (Just pub) person
        person'' = set (keyChain.modulus) (Just modulus_) person'

    in person''

sendMessage :: PP -> PP
sendMessage (a, b) = 
    let b' = set inbound (view outbound a) b
        a' = set outbound Quiet a
    in (a', b')

sendInvitation :: State PP ()
sendInvitation (a, b) = 
    let (aPub, g, p) = fromJust $ do
            aPub <- a ^? keyChain.publicKey
            g <- a ^? keyChain.base
            p <- a ^? keyChain.modulus
            return (aPub, g, p)
        msg = Invitation aPub g p
        a' = a & outbound .~ msg
    return $ sendMessage (a', b)

commonAESKey :: Person -> B.ByteString
commonAESKey p = 
    let key = fromJust $ view (keyChain.commonKey) p 
    in Lib.strictSha1 . Lib.littleEndian64 . fromInteger $ key

receiveInvitation :: State PP ()
receiveInvitation (a, b) = 
    let (aPub, g, p) = fromJust $ do
            aPub <- b ^? inbound.publicKey
            g <- b ^? inbound.base
            p <- b ^? inbound.modulus
            return (aPub, g, p)
        bPriv = view (keyChain.privateKey) b
        commonKey_ = getCommonKey aPub bPriv p

        b' = updateWithBaseAndModulus g p b
           & (keyChain.commonKey) .~ commonKey_ -- may need a just here
    return $ (a, b')

sendAcceptance :: State PP ()
sendAcceptance (a, b) = 
    let bPub = fromJust $ keyChain.publicKey
        msg = Acceptance bPub
        b' = b & outbound .~ msg
    return sendMessage (a, b')

receiveAcceptance :: State PP ()
receiveAcceptance (a, b) =
    let (bPub, g, p) = fromJust $ do
            bPub <- a ^? inbound.publicKey
            p <- a ^? keyChain.modulus
            return (bPub, g, p)
        aPriv = view (keyChain.privateKey) a
        commonKey_ = getCommonKey bPub aPriv p

        a' = a & keyChain.commonKey .~ commonKey_

    return (a', b)


prepareEncryptedMessage 
    :: B.ByteString -> B.ByteString -> Person -> Person
prepareEncryptedMessage info iv_ p =
    let aes = Lib.initAES128 (commonAESKey p)
        content_ = Lib.cbcEncryption aes iv_ info
        msg = Info content_ iv_

        p' = set outbound msg p

    in p'

sendEncryptedMessage :: B.ByteString -> B.ByteString -> PP -> PP
sendEncryptedMessage info iv_ (a, b) = 
    let a' = prepareEncryptedMessage info iv_ a
    in sendMessage (a', b)

receiveEncryptedMessage :: Person -> B.ByteString
receiveEncryptedMessage p =
    let content_ = view (inbound.content) p
        iv_ = view (inbound.iv) p

        aes = Lib.initAES128 (commonAESKey p) 
        info = Lib.cbcDecryption aes iv_ content_
    in info

-- left side is initiator.  Should have g and p properties set already.
echoBot :: State PP (String, String)
echoBot = do
    sendInvitation
    receiveInvitation
    sendAcceptance
    sendEncryptedMessage "hello with my message" "YELLOW SUBMARINE" 
    mAB <- receiveEncryptedMessage
    sendEncryptedMessage' mAB "MELLOW SUBMARINE" 
    mBA <- receiveEncryptedMessage'
    return (mAB, mBA)
    

main :: IO ()
main = do
    let (g, p, aPriv, bPriv) =  (1, 4, 2, 3)
        a = updateWithBaseAndModulus g p $ makePerson "Alice" aPriv
        b = makePerson "Bob" bPriv
    print $ evalState echoBot (a, b)
        
