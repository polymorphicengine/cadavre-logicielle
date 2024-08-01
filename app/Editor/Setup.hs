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
import Data.IORef (newIORef)
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
  messageRef <- liftIO $ newIORef []

  let state = State udp [] messageRef

  playingTable state

-- str <- setupStream
-- (mMV, rMV) <- setupHint mode

-- envMV <- setupBackend str hyd mode mMV rMV
-- loadBootDefs envMV

setupStream :: UI Stream
setupStream = do
  target <- configureTarget
  conf <- configureStream
  liftIO $ startTidal target conf

-- setupHint :: HintMode -> UI (MVar InterpreterMessage, MVar InterpreterResponse)
-- setupHint mode = do
--   mMV <- liftIO newEmptyMVar
--   rMV <- liftIO newEmptyMVar
--   void $ liftIO $ forkIO $ hintJob mode mMV rMV
--   return (mMV, rMV)

-- setupBackend :: Stream -> MVar (Pattern Text) -> HintMode -> MVar InterpreterMessage -> MVar InterpreterResponse -> UI (MVar Environment)
-- setupBackend str hyd mode mMV rMV = do
--   win <- askWindow
--   let env = Environment str (Just $ hyd) defaultTypeEnv (HintEnv mode mMV rMV) (Just $ ConfigEnv (setConfig win) (clearConfig win)) Nothing

--   envMV <- liftIO $ newMVar env

--   return envMV

-- loadBootDefs :: MVar Environment -> UI ()
-- loadBootDefs envMV = do
--   env <- liftIO $ takeMVar envMV
--   mps <- getBootPaths
--   case mps of
--     Just ps -> do
--       x <- liftIO $ runCI env $ compilerInterpreterBoot ps
--       case x of
--         Left (CIError err _) -> do
--           _ <- (getOutputEl # set C.text err)
--           liftIO $ putMVar envMV env
--         Right env' -> do
--           _ <- (getOutputEl # set C.text "successfully loaded boot file(s)!")
--           liftIO $ putMVar envMV env'
--     Nothing -> liftIO $ putMVar envMV env
