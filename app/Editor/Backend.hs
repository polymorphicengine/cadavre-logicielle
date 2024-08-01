module Editor.Backend where

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

import Data.IORef
import Editor.UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Network.Socket as N
import Sound.Osc.Fd as O

-- a player has a name and is identified through their name
data Player = Player {pName :: String, pAddress :: N.SockAddr}

instance Eq Player where
  (==) p1 p2 = pName p1 == pName p2

instance Show Player where
  show = pName

type Players = [Player]

-- state that the playing table contains and acts on
data State
  = State
  { sLocal :: Udp,
    sPlayers :: Players,
    sMessageRef :: IORef [Element]
  }

addPlayer :: Player -> State -> State
addPlayer p st = st {sPlayers = p : sPlayers st}

recvMessageFrom :: Udp -> IO (Maybe Message, N.SockAddr)
recvMessageFrom u = fmap (\(p, n) -> (packet_to_message p, n)) (recvFrom u)

playingTable :: State -> UI ()
playingTable st = do
  m <- liftIO $ recvMessageFrom (sLocal st)
  newSt <- act st m
  playingTable newSt

act :: State -> (Maybe O.Message, N.SockAddr) -> UI State
act st (Just (Message "/ping" []), remote) = do
  addMessage "pinged" (sMessageRef st)
  liftIO $ O.sendTo (sLocal st) (O.p_message "/pong" []) remote
  return st
act st (Just (Message "/sit" [AsciiString x]), remote) = do
  -- TODO: what if player with this name already exists? prevent from connecting multiple times
  addMessage (ascii_to_string x ++ " joined the table!") (sMessageRef st)
  liftIO $ O.sendTo (sLocal st) (O.p_message "/ok" []) remote
  return $ addPlayer (Player (ascii_to_string x) remote) st
act st (Just m, _) = liftIO (putStrLn $ "Unhandled message: " ++ show m) >> return st
act st _ = return st

-- broadcast to all connected players
broadcast :: State -> O.Packet -> IO ()
broadcast st m = mapM_ (O.sendTo (sLocal st) m . pAddress) (sPlayers st)
