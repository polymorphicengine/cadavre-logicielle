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
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Monad (void)
import Game.Backend
import Game.Config
import Game.Frontend
import Game.Hint
import Game.Types
import Game.UI
import Graphics.UI.Threepenny.Core as C hiding (defaultConfig, text)
import qualified Sound.Osc.Transport.Fd.Udp as O
import Sound.Tidal.Stream

setup :: Window -> UI ()
setup win = void $ do
  frontend win
  str <- setupStream
  (mMV, rMV) <- setupHint str

  -- listening address
  udp <- liftIO $ O.udp_server 2323

  let state = State udp [] [] mMV rMV str
  _ <- liftIO $ forkIO $ runUI win $ void $ runGame state playingTable
  addMessage "Succesfully prepared the table. Ready to host the game."

setupStream :: UI Stream
setupStream = do
  target <- configureTarget
  conf <- configureStream
  liftIO $ startTidal target conf

setupHint :: Stream -> UI (MVar InterpreterMessage, MVar InterpreterResponse)
setupHint stream = do
  mMV <- liftIO newEmptyMVar
  rMV <- liftIO newEmptyMVar
  void $ liftIO $ forkIO $ hintJob stream mMV rMV
  return (mMV, rMV)
