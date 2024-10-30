module Game.Config where

import Data.Text (Text, pack, unpack)
import Graphics.UI.Threepenny.Core
import qualified Sound.Tidal.Clock as Clock
import Sound.Tidal.Context hiding ((#))
import Sound.Tidal.Stream.Config as Conf
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)

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
            (if bb then return $ Just [pack p] else return Nothing)
        )
    )
