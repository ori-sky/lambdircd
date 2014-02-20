module IRC.Server.Options where

data Options = Options
    { port              :: Int
    , connectTimeout    :: Int
    , pingTimeout       :: Int
    , plugins           :: [String]
    }

defaultOptions :: Options
defaultOptions = Options
    { port              = 6667
    , connectTimeout    = 20
    , pingTimeout       = 240
    , plugins           = []
    }
