{- Copyright 2014 David Farrell <shokku.ra@gmail.com>

 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at

 - http://www.apache.org/licenses/LICENSE-2.0

 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module IRC.Message where

import Data.Char
import IRC.Prefix

data Message = Message
    { prefix    :: Maybe Prefix
    , command   :: String
    , params    :: [String]
    } deriving (Show, Eq)

ircParams :: String -> [String]
ircParams "" = []
ircParams (':':xs) = [xs]
ircParams s = x : (ircParams $ drop 1 xs)
  where (x, xs) = break isSpace s

parseMessage :: String -> Message
parseMessage "" = Message Nothing "" []
parseMessage (':':xs)
    | prefix' == "" = msg {prefix=Nothing}
    | otherwise     = do
        msg {prefix=Just (StringPrefix prefix')}
  where
    (prefix', rest) = break isSpace xs
    msg = parseMessage rest
parseMessage (' ':':':xs) = parseMessage xs
parseMessage (' ':xs) = parseMessage xs
parseMessage line = Message Nothing (map toUpper command) $ ircParams (unwords params)
  where command:params = words line
