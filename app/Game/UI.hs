{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Game.UI where

{-
    UI.hs - miscellanious functions for the user interface
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
import Data.Text (Text, pack, unpack)
import Data.Time
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (get, text, value)
import qualified Sound.Tidal.Clock as Clock
import Sound.Tidal.Context hiding ((#))
import Sound.Tidal.Stream.Config as Conf
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

createHaskellFunction name fn = do
  handler <- ffiExport fn
  runFunction $ ffi ("window." ++ name ++ " = %1") handler

catchJSErrors :: UI ()
catchJSErrors = runFunction $ ffi "window.onerror = function(msg, url, linenumber) { alert(msg);return true;}"

setConfig :: Window -> Text -> Text -> IO ()
setConfig win key v = runUI win $ runFunction $ ffi ("window.electronAPI.putInStore(%1," ++ (unpack v) ++ ")") (unpack key)

clearConfig :: Window -> IO ()
clearConfig win = runUI win $ runFunction $ ffi "window.electronAPI.clearStore()"

configureTarget :: UI Target
configureTarget = do
  dirtport <- callFunction $ ffi "fullSettings.tidal.dirtport"
  latency <- callFunction $ ffi "fullSettings.tidal.latency"
  return $ superdirtTarget {oLatency = latency, oAddress = "127.0.0.1", oPort = dirtport}

configureStream :: UI Conf.Config
configureStream = do
  frameTimespan <- callFunction $ ffi "fullSettings.tidal.frameTimespan"
  processAhead <- callFunction $ ffi "fullSettings.tidal.processAhead"
  link <- callFunction $ ffi "fullSettings.tidal.link"
  let linkB = case link of
        "false" -> False
        _ -> True
  skipTicks <- callFunction $ ffi "fullSettings.tidal.skipTicks"
  quantum <- callFunction $ ffi "fullSettings.tidal.quantum"
  beatsPerCycle <- callFunction $ ffi "fullSettings.tidal.beatsPerCycle"
  return $
    Conf.defaultConfig
      { cVerbose = False,
        cClockConfig =
          Clock.defaultConfig
            { Clock.cFrameTimespan = frameTimespan,
              Clock.cEnableLink = linkB,
              Clock.cProcessAhead = processAhead,
              Clock.cSkipTicks = read skipTicks,
              Clock.cQuantum = read quantum,
              Clock.cBeatsPerCycle = read beatsPerCycle
            }
      }

getBootPaths :: UI (Maybe [Text])
getBootPaths = do
  p <- callFunction $ ffi "fullSettings.bootPath"
  b <- liftIO $ doesDirectoryExist p
  ( if b
      then fmap (Just . map (\x -> pack $ p ++ "/" ++ x)) $ liftIO $ listDirectory p
      else
        ( do
            bb <- liftIO $ doesFileExist p
            (if bb then return $ Just [pack p] else addMessage (show p) >> return Nothing)
        )
    )

wrapCatchErr :: String -> String
wrapCatchErr st = "try {" ++ st ++ "} catch (err) {}"

mkMessage :: String -> String -> UI Element
mkMessage t m = UI.pre # set UI.text (t ++ " - " ++ m) #. "message"

addElement :: String -> String -> Element -> UI ()
addElement className containerId el = do
  win <- askWindow
  els <- getElementsByClassName win className
  mayContainer <- getElementById win containerId
  case mayContainer of
    Nothing -> return ()
    Just cont -> void $ element cont # set UI.children (el : els)

addMessage :: String -> UI ()
addMessage m = do
  t <- liftIO getZonedTime
  el <- mkMessage (showTime t) m
  addElement "message" "message-container" el

showTime :: ZonedTime -> String
showTime = take 8 . show . localTimeOfDay . zonedTimeToLocalTime

infixl 8 #@

(#@) :: UI Element -> String -> UI Element
(#@) mx s = mx # set (attr "id") s
