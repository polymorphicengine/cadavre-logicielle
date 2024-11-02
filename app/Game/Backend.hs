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

playingTable :: Game ()
playingTable = recvMessageFrom >>= act >> playingTable

recvMessageFrom :: Game (Maybe Message, RemoteAddress)
recvMessageFrom = gets sLocal >>= \u -> liftIO $ fmap (first packet_to_message) (recvFrom u)

act :: (Maybe O.Message, RemoteAddress) -> Game ()
act (Just (Message "/ping" []), remote) = pingAction remote
act (Just (Message "/say" [AsciiString x]), remote) = sayAction (toUTF8 x) remote
act (Just (Message "/sit" [AsciiString x, Int32 orb]), remote) = sitAction (toUTF8 x) (fromIntegral orb) remote
act (Just (Message "/define" [AsciiString n, AsciiString t, AsciiString c, AsciiString d]), remote) = defineAction (toUTF8 n) (toUTF8 t) (toUTF8 c) (toUTF8 d) remote
act (Just (Message "/eval" [AsciiString stat]), remote) = evaluateStatement (toUTF8 stat) remote
act (Just (Message "/type" [AsciiString typ]), remote) = typeAction (toUTF8 typ) remote
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
  broadcast (p_message "/joined" [utf8String name, O.Int32 $ fromIntegral orb])

defineAction :: String -> String -> String -> String -> RemoteAddress -> Game ()
defineAction name typ code def = addDefinition (Definition name typ code def)

unhandledAction :: RemoteAddress -> Game ()
unhandledAction remote = do
  name <- getNameFromAddress remote
  liftUI $ addMessage (name ++ " gave an unkown instruction to the table.")
  replyError "unkown instruction!" remote
