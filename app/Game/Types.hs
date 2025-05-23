{-# LANGUAGE FlexibleInstances #-}

module Game.Types where

import Control.Concurrent (MVar)
import Control.Monad.State (StateT (..), lift)
import Graphics.UI.Threepenny (MonadUI, UI)
import Graphics.UI.Threepenny.Core (MonadUI (..))
import qualified Network.Socket as N
import Sound.Osc.Transport.Fd.Udp (Udp)
import Zwirn.Language.Compiler

type RemoteAddress = N.SockAddr

-- a player has a name and an address
data Player
  = Player
  { pName :: String,
    pAddress :: RemoteAddress,
    pCode :: String,
    pOrbit :: Int
  }

instance Eq Player where
  (==) p1 p2 = pName p1 == pName p2

instance Show Player where
  show = pName

-- a definition consists of a name, type and code
data Definition
  = Definition
  { dName :: String,
    dType :: String,
    dCode :: String
  }

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
    sEnv :: MVar Environment
  }

type Game = StateT State UI

instance MonadUI Game where
  liftUI = lift . liftUI

runGame :: State -> Game a -> UI (a, State)
runGame st g = runStateT g st
