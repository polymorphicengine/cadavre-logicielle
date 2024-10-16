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
import Game.Game
import Game.UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import Sound.Osc.Fd as O

playingTable :: Game ()
playingTable = recvMessageFrom >>= act >> playingTable

recvMessageFrom :: Game (Maybe Message, RemoteAddress)
recvMessageFrom = gets sLocal >>= \u -> liftIO $ fmap (first packet_to_message) (recvFrom u)

act :: (Maybe O.Message, RemoteAddress) -> Game ()
act (Just (Message "/ping" []), remote) = pingAction remote
act (Just (Message "/say" [AsciiString x]), remote) = sayAction (ascii_to_string x) remote
act (Just (Message "/sit" [AsciiString x]), remote) = sitAction (ascii_to_string x) remote
act (Just (Message "/define" [AsciiString n, AsciiString t, AsciiString c, AsciiString d]), remote) = defineAction (ascii_to_string n) (ascii_to_string t) (ascii_to_string c) (ascii_to_string d) remote
act (Just (Message "/eval" [AsciiString stat]), remote) = evaluateStatement (ascii_to_string stat) remote
act (Just (Message "/type" [AsciiString typ]), remote) = getType (ascii_to_string typ) remote
act (Just _, remote) = unhandledAction remote
act _ = return ()

-- broadcast to all connected players
broadcast :: O.Packet -> Game ()
broadcast m = do
  ps <- gets sPlayers
  local <- gets sLocal
  mapM_ (liftIO . O.sendTo local m . pAddress) ps

pingAction :: RemoteAddress -> Game ()
pingAction remote = do
  replyOK remote
  name <- getNameFromAddress remote
  liftUI $ addMessage (name ++ " pinged the table!")

sayAction :: String -> RemoteAddress -> Game ()
sayAction say remote = do
  replyOK remote
  name <- getNameFromAddress remote
  broadcast (p_message "/say" [O.string name, O.string say])
  liftUI $ addMessage (name ++ " says " ++ say)

sitAction :: String -> RemoteAddress -> Game ()
sitAction name remote = addPlayer (Player name remote "") >> replyOK remote

defineAction :: String -> String -> String -> String -> RemoteAddress -> Game ()
defineAction name typ code def remote = broadcast (p_message "/define" [O.string name, O.string typ]) >> addDefinition (Definition name typ code def) remote

unhandledAction :: RemoteAddress -> Game ()
unhandledAction remote = do
  name <- getNameFromAddress remote
  liftUI $ addMessage (name ++ " gave an unkown instruction to the table.")
  replyError "unkown instruction!" remote
