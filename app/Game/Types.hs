{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game.Types where

import Control.Concurrent (MVar)
import Control.Monad.State (StateT (..), lift)
import Game.Hint
import Graphics.UI.Threepenny (MonadUI, UI)
import Graphics.UI.Threepenny.Core (MonadUI (..))
import qualified Network.Socket as N
import Sound.Osc (Udp)
import Sound.Tidal.Stream (Stream)

type RemoteAddress = N.SockAddr

-- a player has a name and an address
data Player
  = Player
  { pName :: String,
    pAddress :: RemoteAddress,
    pCode :: String
  }

instance Eq Player where
  (==) p1 p2 = pName p1 == pName p2

instance Show Player where
  show = pName

-- a definition consists of a name, type, code and def
data Definition
  = Definition
  { dName :: String,
    dType :: String,
    dCode :: String,
    dDef :: String
  }

instance Show Definition where
  show (Definition n t _ _) = n ++ " :: " ++ t

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

type Game = StateT State UI

instance MonadUI Game where
  liftUI = lift . liftUI

runGame :: State -> Game a -> UI (a, State)
runGame st g = runStateT g st
