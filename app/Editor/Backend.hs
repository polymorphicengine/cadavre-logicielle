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

import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Text (pack)
import Editor.UI
import Foreign.JavaScript (JSObject)
import qualified Graphics.UI.Threepenny as UI
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

recvMessageFrom :: Udp -> IO (Maybe Message, N.SockAddr)
recvMessageFrom u = fmap (\(p, n) -> (packet_to_message p, n)) (recvFrom u)

-- | Start Haskell interpreter, with input and output mutable variables to
-- communicate with it
serve :: IO ()
serve = do
  putStrLn "starting server"
  local <- udpServer "127.0.0.1" 2323

  loop local
  where
    loop l =
      do
        m <- recvMessageFrom l
        act l m
        loop l

act :: Udp -> (Maybe O.Message, N.SockAddr) -> IO ()
-- test if the listener is responsive
act l (Just (Message "/ping" []), remote) = print remote >> O.sendTo l (O.p_message "/pong" []) remote
act _ (Nothing, _) = putStrLn "Not a message?"
act _ (Just m, _) = putStrLn $ "Unhandled message: " ++ show m
