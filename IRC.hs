module IRC
( Hostmask(..)
, Prefix(..)
, Message(..)
, ircParams
, parseMessage
) where

import Data.Char
import LeftApplication

data Hostmask = Hostmask
    { nick  :: String
    , user  :: String
    , host  :: String
    }

instance Show Hostmask where
    show (Hostmask n u h) = n ++ ('!':u) ++ ('@':h)

data Prefix = StringPrefix String | MaskPrefix Hostmask
instance Show Prefix where
    show (StringPrefix s)   = s
    show (MaskPrefix m)     = show m

data Message = Message
    { prefix    :: Maybe Prefix
    , command   :: String
    , params    :: [String]
    } deriving (Show)

ircParams :: String -> [String]
ircParams "" = []
ircParams (':':xs) = [xs]
ircParams s = x : (ircParams $ drop 1 xs)
  where (x, xs) = break isSpace s

parseMessage :: String -> Message
parseMessage "" = Message Nothing "" []
parseMessage (':':s) = Message
    $> Just (StringPrefix $ (head.words) s)
    $> (head.tail.ircParams) s
    $> (tail.tail.ircParams) s
parseMessage s = Message Nothing
    $> (head.ircParams) s
    $> (tail.ircParams) s
