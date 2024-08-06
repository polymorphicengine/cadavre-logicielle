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

import Control.Concurrent.MVar
import Data.Bifunctor
import Data.Maybe (fromMaybe)
import Game.Hint
import Game.UI
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import qualified Network.Socket as N
import Sound.Osc.Fd as O
import Sound.Tidal.Context (Stream)

-- a player has a name and an address
data Player = Player {pName :: String, pAddress :: N.SockAddr}

instance Eq Player where
  (==) p1 p2 = pName p1 == pName p2

instance Show Player where
  show = pName

-- a definition consists of a name, type and code
data Definition = Definition {dName :: String, dType :: String, dCode :: String}

instance Show Definition where
  show (Definition n t _) = n ++ " :: " ++ t

type Definitions = [Definition]

type Players = [Player]

-- state that the playing table contains and acts on
data State
  = State
  { sLocal :: Udp,
    sPlayers :: Players,
    sDefinitions :: Definitions,
    sHintMessage :: MVar InterpreterMessage,
    sHintResponse :: MVar InterpreterResponse,
    sStream :: Stream
  }

getNameFromAddress :: Players -> N.SockAddr -> String
getNameFromAddress ps add = fromMaybe "unkown player" (lookup add ads)
  where
    ads = map (\(Player n a) -> (a, n)) ps

getTypeFromName :: Definitions -> String -> String
getTypeFromName ds n = fromMaybe "unkown type" (lookup n ns)
  where
    ns = map (\(Definition m x _) -> (m, x)) ds

addPlayer :: Player -> State -> UI State
addPlayer p st =
  if pName p `elem` map pName (sPlayers st)
    then return st
    else
      ( do
          el <- mkPlayer p
          addMessage (pName p ++ " joined the table!")
          addElement "player" "player-container" el
          return $ st {sPlayers = p : sPlayers st}
      )

mkPlayer :: Player -> UI Element
mkPlayer p = UI.p #. "player" #@ playerID p # set UI.text (pName p)

playerID :: Player -> String
playerID p = "player-" ++ pName p

mkDefinition :: Definition -> UI Element
mkDefinition d = UI.p #. "definition" #@ defID d # set UI.text (show d)

defID :: Definition -> String
defID d = "def-" ++ dName d

addDefinition :: String -> Definition -> State -> N.SockAddr -> UI State
addDefinition name d st remote =
  if dName d `elem` map dName (sDefinitions st)
    then
      -- already defined variables cannot change type
      if dType d /= getTypeFromName (sDefinitions st) (dName d)
        then return st
        else
          ( do
              liftIO $ putMVar (sHintMessage st) (MStat $ dCode d)
              r <- liftIO $ takeMVar (sHintResponse st)
              case r of
                RStat Nothing -> do
                  liftIO $ O.sendTo (sLocal st) (O.p_message "/ok" []) remote
                  addMessage (name ++ " changed the definition of " ++ dName d)
                RError e -> liftIO $ O.sendTo (sLocal st) (O.p_message "/error" [O.string e]) remote
                _ -> return ()
              return st
          )
    else
      ( do
          el <- mkDefinition d
          addMessage (name ++ " folded the document and revealed " ++ show d)
          addElement "definition" "definition-container" el
          -- TODO: actually interpret the definition code
          return $ st {sDefinitions = d : sDefinitions st}
      )

recvMessageFrom :: Udp -> IO (Maybe Message, N.SockAddr)
recvMessageFrom u = fmap (first packet_to_message) (recvFrom u)

playingTable :: State -> UI ()
playingTable st = do
  m <- liftIO $ recvMessageFrom (sLocal st)
  newSt <- act st m
  playingTable newSt

act :: State -> (Maybe O.Message, N.SockAddr) -> UI State
act st (Just (Message "/ping" []), remote) = do
  liftIO $ O.sendTo (sLocal st) (O.p_message "/ok" []) remote
  addMessage (getNameFromAddress (sPlayers st) remote ++ " pinged the table!")
  return st
act st (Just (Message "/say" [AsciiString x]), remote) = do
  liftIO $ O.sendTo (sLocal st) (O.p_message "/ok" []) remote
  liftIO $ broadcast st (p_message "/say" [AsciiString x])
  addMessage (getNameFromAddress (sPlayers st) remote ++ " says " ++ ascii_to_string x)
  return st
act st (Just (Message "/sit" [AsciiString x]), remote) = do
  liftIO $ O.sendTo (sLocal st) (O.p_message "/ok" []) remote
  addPlayer (Player (ascii_to_string x) remote) st
act st (Just (Message "/define" [AsciiString n, AsciiString t, AsciiString c]), remote) = do
  liftIO $ broadcast st (p_message "/define" [AsciiString n, AsciiString t])
  addDefinition (getNameFromAddress (sPlayers st) remote) (Definition (ascii_to_string n) (ascii_to_string t) (ascii_to_string c)) st remote
act st (Just (Message "/eval" [AsciiString statement]), remote) = do
  liftIO $ putMVar (sHintMessage st) (MStat $ ascii_to_string statement)
  r <- liftIO $ takeMVar (sHintResponse st)
  case r of
    RStat (Just x) -> do
      liftIO $ O.sendTo (sLocal st) (O.p_message "/eval/value" [O.string x]) remote
      addMessage (getNameFromAddress (sPlayers st) remote ++ " evaluated a statement with value " ++ x)
    RStat Nothing -> do
      liftIO $ O.sendTo (sLocal st) (O.p_message "/ok" []) remote
      addMessage (getNameFromAddress (sPlayers st) remote ++ " evaluated a statement!")
    RError e -> do
      liftIO $ O.sendTo (sLocal st) (O.p_message "/error" [O.string e]) remote
      addMessage (getNameFromAddress (sPlayers st) remote ++ " made an error: \n" ++ e)
    _ -> return ()
  return st
act st (Just m, _) = liftIO (putStrLn $ "Unhandled message: " ++ show m) >> return st
act st _ = return st

-- broadcast to all connected players
broadcast :: State -> O.Packet -> IO ()
broadcast st m = mapM_ (O.sendTo (sLocal st) m . pAddress) (sPlayers st)
