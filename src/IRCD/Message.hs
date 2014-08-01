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

module IRCD.Message (parseMessage) where

import Data.Char (isSpace, toUpper)
import IRCD.Types

parseMessage :: String -> Message
parseMessage "" = Message () Nothing "" []
parseMessage (':':xs)
    | null prefix' = msg {prefix=Nothing}
    | otherwise = msg {prefix=Just (StringPrefix prefix')}
  where (prefix', rest) = break isSpace xs
        msg = parseMessage rest
parseMessage (' ':':':xs) = parseMessage xs
parseMessage (' ':xs) = parseMessage xs
parseMessage line = Message () Nothing (map toUpper cmd) (parseParams rest)
  where (cmd, rest) = break isSpace line

parseParams :: String -> [String]
parseParams "" = []
parseParams (' ':xs) = parseParams xs
parseParams (':':xs) = [xs]
parseParams str = x : parseParams (drop 1 xs)
  where (x, xs) = break isSpace str
