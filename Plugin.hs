{-# LANGUAGE RankNTypes #-}

module Plugin where

data Interface = Interface
    { isPlugin  :: Bool
    , name      :: String
    }

defaultPlugin = Interface
    { isPlugin  = True
    , name      = ""
    }
