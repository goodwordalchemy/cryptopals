{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Data.ByteString as B
import DiffieHellman( genPublicKey
                    , getCommonKey
                    ) 

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
                , _commonKey :: Maybe Integer
                } deriving (Show)

data Message = Invitation 
                { _publicKey :: Integer
                , _base :: Integer
                , _modulus :: Integer
                } 
             | Acceptance
                { _publicKey :: Integer }
             | Info
                { _content :: B.ByteString
                , _iv :: B.ByteString
                } deriving (Show)

$(makeLenses ''Person)
$(makeLenses ''KeyChain)

makePerson :: String -> Integer -> Person
makePerson name privateKey = 
    let kc = KeyChain Nothing Nothing privateKey Nothing Nothing
    in Person {_name = name, _keyChain = kc, message = Nothing}

updateWithBaseAndModulus :: Integer -> Integer -> Person -> Person
updateWithBaseAndModulus base_ modulus_ person =
    let priv = view (keyChain.privateKey) person
        pub = genPublicKey base_ priv modulus_
    in set (keyChain.publicKey) pub

sendMessage :: PP -> PP
sendMessage a b = 
    let b' = set inbound (view outbound a) b
        a' = set outbound B.empty a
    in (a', b')

sendInvitation :: Integer -> Integer -> PP -> PP
sendInvitation base_ modulus_ (a, b) =
    let aPub = view (keyChain.publicKey) a
        msg = Invitation aPub base_ modulus_
        a' = set outbound msg a
    in sendMessage (a', b)

receiveInvitation :: Person -> Person
receiveInvitation b = 
    let g = view (inbound.base) b
        p = view (inbound.modulus) b
        b' = updateWithBaseAndModulus g p b

        aPub = view (inbound.publicKey) b'
        bPriv = view (keyChain.privateKey) b'
        commonKey_ = getCommonKey aPub bPriv

        b'' = set (keyChain.commonKey) (Just commonKey_)

    in b''

sendAcceptance :: PP -> PP
sendAcceptance (b, a) = 
    let bPub = view (keyChain.publicKey) b
        msg = Acceptance bPub
        b' = set outbound msg b
    in sendMessage (b', a)

receiveAcceptance :: Person -> Person
receiveAcceptance a =
    let bPub = view (inbound.publicKey) a
        aPriv = view (keyChain.privateKey) a
        commonKey_ = getCommonKey bPub aPriv
        
        b' = set (keyChain.commonKey) (Just commonKey_)

    in b'

commonKey' :: Person -> Integer
commonKey' p = 
    let keyMaybe = view (keyChain.commonKey) p
    in case keyMaybe of 
        Nothing -> error $ "can't send messages without key exchange first for person: " ++ show p
        Just k -> k


prepareEncryptedMessage 
    :: B.ByteString -> B.ByteString -> Person -> Person
prepareEncryptedMessage info iv_ p =
    let aes = Lib.initAES (commonKey' p)
        content_ = Lib.cbcEncryption aes iv_ info
        msg = Info content_ iv_

        p' = set outbound msg p

    in p'

sendEncryptedMessage :: B.ByteString -> B.ByteString -> PP -> PP
sendEncryptedMessage info iv_ (a, b) = 
    let a' = prepareEncryptedMessage info iv_ a
    in sendMessage (a', b)

receiveEncryptedMesage :: Person -> B.ByteString
receiveEncryptedMessage p
    let content_ = view (inbound.content) p
        iv_ = view (inbound.iv) p

        aes = Lib.initAES (commonKey' p) 
        info = Lib.cbcDecryption aes iv_ content_
    in info
    
