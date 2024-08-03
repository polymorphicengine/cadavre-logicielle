{-# LANGUAGE OverloadedStrings #-}

module Editor.Setup (setup) where

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

import Control.Monad (void)
import Editor.Backend
import Editor.Frontend
import Editor.UI
import Graphics.UI.Threepenny.Core as C hiding (defaultConfig, text)
import Sound.Osc.Fd as O
import Sound.Tidal.Stream

setup :: Window -> UI ()
setup win = void $ do
  frontend win

  udp <- liftIO $ udpServer "127.0.0.1" 2323

  let state = State udp [] []
  playingTable state

-- str <- setupStream
-- hint <- setupHint

-- state <- setupBackend str hint

setupStream :: UI Stream
setupStream = do
  target <- configureTarget
  conf <- configureStream
  liftIO $ startTidal target conf

-- setupHint :: UI (MVar InterpreterMessage, MVar InterpreterResponse)
-- setupHint = do
--   mMV <- liftIO newEmptyMVar
--   rMV <- liftIO newEmptyMVar
--   void $ liftIO $ forkIO $ hintJob mode mMV rMV
--   return (mMV, rMV)
