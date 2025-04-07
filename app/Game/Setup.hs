{-# LANGUAGE OverloadedStrings #-}

module Game.Setup (setup) where

{-
    Setup.hs - setup of the various components of the backend
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

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (void)
import qualified Data.Map as Map
import Game.Backend
import Game.Frontend
import Game.Types
import Game.UI
import Graphics.UI.Threepenny.Core as C hiding (defaultConfig, text)
import qualified Sound.Osc.Transport.Fd.Udp as O
import qualified Sound.Tidal.Clock as Clock (defaultConfig)
import Zwirn.Language.Builtin.Prelude
import Zwirn.Language.Compiler
import Zwirn.Stream.Types (Stream (..), StreamConfig (..))
import Zwirn.Stream.UI

setup :: Window -> UI ()
setup win = void $ do
  frontend win
  str <- setupStream
  -- listening address
  udp <- liftIO $ O.udp_server 2323

  let env = Environment str builtinEnvironment builtinEnvironment Nothing Nothing (CiConfig False False) Nothing

  envMV <- liftIO $ newMVar env

  let state = State udp [] [] envMV

  _ <- liftIO $ forkIO $ runUI win $ void $ runGame state playingTable
  addMessage "Succesfully prepared the table. Ready to host the game."

setupStream :: UI Stream
setupStream = do
  mv <- liftIO $ newMVar Map.empty
  m <- liftIO $ newMVar Map.empty
  liftIO $ startStream (StreamConfig 57120 57110 "127.0.0.1" Clock.defaultConfig) mv m
