module IRC
( Hostmask(..)
, Prefix(..)
, Message(..)
) where

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
