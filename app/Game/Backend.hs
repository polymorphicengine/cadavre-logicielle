module Game.Backend where

{-
    Backend.hs - Implements the interaction between the compiler-interpreter and the editor
    Copyright (C) 2023, Martin Gius

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import Control.Monad.State (gets)
import Data.Bifunctor
import Game.Actions
import Game.Types
import Game.UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import Sound.Osc.Fd as O
import qualified Sound.Osc.Transport.Fd.Udp as O

playingTable :: Game ()
playingTable = recvMessageFrom >>= act >> playingTable

recvMessageFrom :: Game (Maybe Message, RemoteAddress)
recvMessageFrom = gets sLocal >>= \u -> liftIO $ fmap (first packet_to_message) (O.recvFrom u)

act :: (Maybe O.Message, RemoteAddress) -> Game ()
act (Just (Message "/ping" []), remote) = pingAction remote
act (Just (Message "/say" [AsciiString x]), remote) = sayAction (toUTF8 x) remote
act (Just (Message "/sit" [AsciiString x, Int32 orb]), remote) = sitAction (toUTF8 x) (fromIntegral orb) remote
act (Just (Message "/define" [AsciiString name, AsciiString code]), remote) = defineAction (toUTF8 name) (toUTF8 code) remote
act (Just (Message "/set" [AsciiString name, AsciiString code]), remote) = setAction (toUTF8 name) (toUTF8 code) remote
act (Just (Message "/eval" [AsciiString stat]), remote) = evalAction (toUTF8 stat) remote
act (Just _, remote) = unhandledAction remote
act _ = return ()

pingAction :: RemoteAddress -> Game ()
pingAction remote = do
  replyOK remote
  name <- getNameFromAddress remote
  liftUI $ addMessage (name ++ " pinged the table!")

sayAction :: String -> RemoteAddress -> Game ()
sayAction say remote = do
  replyOK remote
  name <- getNameFromAddress remote
  broadcast (p_message "/message" [utf8String (name ++ " says " ++ say)])
  liftUI $ addMessage (name ++ " says " ++ say)

sitAction :: String -> Int -> RemoteAddress -> Game ()
sitAction name orb remote = do
  addPlayer (Player name remote "" orb)
  replyOK remote

defineAction :: String -> String -> RemoteAddress -> Game ()
defineAction = addDefinition Define

setAction :: String -> String -> RemoteAddress -> Game ()
setAction = addDefinition Set

evalAction :: String -> RemoteAddress -> Game ()
evalAction stat remote = do
  mayv <- evaluateStatement stat
  case mayv of
    Right (Just v) -> updateCode stat remote >> replyOKVal v remote
    Right Nothing -> updateCode stat remote >> replyOK remote
    Left e -> replyError e remote

unhandledAction :: RemoteAddress -> Game ()
unhandledAction remote = do
  name <- getNameFromAddress remote
  liftUI $ addMessage (name ++ " gave an unkown instruction to the table.")
  replyError "unkown instruction!" remote
