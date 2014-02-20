module IRC.Server.MessageHandler where

import IRC
import IRC.Server.Options
import IRC.Server.Client

type MessageHandler = Options -> Client -> Message -> IO Client

