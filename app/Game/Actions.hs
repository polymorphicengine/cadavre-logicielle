module Game.Actions where

import Control.Concurrent (putMVar, takeMVar)
import Control.Monad (unless, void)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Game.Types
import Game.UI
import Graphics.UI.Threepenny as UI (Element, UI, askWindow, element, getElementById, liftUI, set, text, (#))
import Sound.Osc.Fd as O
import qualified Sound.Osc.Transport.Fd.Udp as O
import Zwirn.Language.Compiler

--------------------------------------------------------
------------------- player actions ---------------------
--------------------------------------------------------

addPlayer :: Player -> Game ()
addPlayer p = do
  ps <- gets sPlayers
  if pAddress p `elem` map pAddress ps
    then renamePlayer (pName p) (pAddress p)
    else do
      unless
        (pName p `elem` map pName ps)
        ( do
            el <- liftUI $ mkPlayer p
            liftUI $ addElement "player" "player-container" el
            liftUI $ addMessage (pName p ++ " joined the table!")
            sharePlayers p
            shareDefinitions p
            broadcast (O.p_message "/joined" [utf8String (pName p), O.Int32 $ fromIntegral (pOrbit p)])
            modify $ \st -> st {sPlayers = p : sPlayers st}
        )

renamePlayer :: String -> RemoteAddress -> Game ()
renamePlayer new add = do
  mayp <- getPlayerFromAddress add
  case mayp of
    Nothing -> return ()
    Just p -> do
      el <- liftUI $ getNameElement p
      void $ liftUI $ element el # set text new
      liftUI $ addMessage (pName p ++ " renamed themselves to " ++ new)
      broadcast (O.p_message "/rename" [utf8String (pName p), utf8String new])
      modify $ \st -> st {sPlayers = map (\q -> if pAddress q == add then q {pName = new} else q) $ sPlayers st}

shareDefinitions :: Player -> Game ()
shareDefinitions p = do
  ds <- gets sDefinitions
  mapM_ (\d -> reply (pAddress p) (O.p_message "/define" [utf8String (pName p), utf8String $ dName d, utf8String $ dType d])) ds

sharePlayers :: Player -> Game ()
sharePlayers p = do
  ps <- gets sPlayers
  mapM_ (\x -> reply (pAddress p) (O.p_message "/joined" [utf8String (pName x), O.Int32 (fromIntegral $ pOrbit x)])) ps

--------------------------------------------------------
----------------- definition actions -------------------
--------------------------------------------------------

data DefinitionMode = Set | Define deriving (Eq)

addDefinition :: DefinitionMode -> String -> String -> RemoteAddress -> Game ()
addDefinition mode name code remote = do
  first <- isFirstDefinition name
  may <- (if mode == Set then setDefinition else defineDefinition) name code

  case may of
    Left e -> replyError e remote
    Right _ -> do
      replyOK remote
      pname <- getNameFromAddress remote
      if first
        then do
          typ <- getType name
          let d = Definition name typ code

          -- add the element
          el <- liftUI $ mkDefinition d
          liftUI $ addElement "definition" "definition-container" el
          modify $ \st -> st {sDefinitions = d : sDefinitions st}

          -- add and broadcast message
          liftUI (addMessage (pname ++ " folded the document and revealed " ++ show d))
          broadcast (O.p_message "/define" [utf8String pname, utf8String $ dName d, utf8String $ dType d])
        else do
          -- update code of definition
          ds <- gets sDefinitions
          let newds = map (\x@(Definition n _ _) -> if n == name then x {dCode = code} else x) ds
          modify $ \st -> st {sDefinitions = newds}
          -- add and broadcast message
          liftUI (addMessage (pname ++ " changed the definition of " ++ name))
          broadcast (O.p_message "/change" [utf8String pname, utf8String name])

getType :: String -> Game String
getType name = do
  eith <- evaluateStatement (":t " ++ name)
  case eith of
    Right (Just x) -> return $ drop (length name + 4) x
    _ -> error "Error in typecheck of definition name."

isFirstDefinition :: String -> Game Bool
isFirstDefinition name = do
  ds <- gets sDefinitions
  case lookup name (map (\x@(Definition n _ _) -> (n, x)) ds) of
    Just _ -> return False
    Nothing -> return True

setDefinition :: String -> String -> Game (Either String ())
setDefinition name code = do
  eith <- evaluateStatement (name ++ " <- " ++ code)
  case eith of
    Left err -> return $ Left err
    Right _ -> return $ Right ()

defineDefinition :: String -> String -> Game (Either String ())
defineDefinition name code = do
  eith <- evaluateStatement (name ++ " = " ++ code)
  case eith of
    Left err -> return $ Left err
    Right _ -> return $ Right ()

--------------------------------------------------------
-------------------- code actions ----------------------
--------------------------------------------------------

-- returns left on success
evaluateStatement :: String -> Game (Either String (Maybe String))
evaluateStatement stat = do
  envMV <- gets sEnv
  env <- liftIO $ takeMVar envMV
  resp <- liftIO $ runCI env (compilerInterpreterBasic (pack stat) >>= (\r -> get >>= \e -> return (r, e)))
  case resp of
    Left (CIError err newEnv) -> do
      liftIO (putMVar envMV newEnv)
      return $ Left err
    Right ("", newEnv) -> do
      liftIO (putMVar envMV newEnv)
      return $ Right Nothing
    Right (x, newEnv) -> do
      liftIO (putMVar envMV newEnv)
      return $ Right (Just x)

updateCode :: String -> RemoteAddress -> Game ()
updateCode code add = do
  mayp <- getPlayerFromAddress add
  case mayp of
    Nothing -> return ()
    Just p -> do
      el <- liftUI $ getCodeElement p
      void $ liftUI $ element el # set text code

--------------------------------------------------------
------------------ helper functions --------------------
--------------------------------------------------------

getCodeElement :: Player -> UI Element
getCodeElement p = do
  win <- askWindow
  elMay <- getElementById win (codeID p)
  case elMay of
    Nothing -> mkCode p
    Just el -> return el

getNameElement :: Player -> UI Element
getNameElement p = do
  win <- askWindow
  elMay <- getElementById win (nameID p)
  case elMay of
    Nothing -> mkName p
    Just el -> return el

getPlayerFromAddress :: RemoteAddress -> Game (Maybe Player)
getPlayerFromAddress add = do
  ps <- gets sPlayers
  return $ lookup add (map (\p@(Player _ a _ _) -> (a, p)) ps)

getNameFromAddress :: RemoteAddress -> Game String
getNameFromAddress = fmap (maybe "unkown player" pName) . getPlayerFromAddress

getTypeFromName :: Definitions -> String -> String
getTypeFromName ds n = fromMaybe "unkown type" (lookup n ns)
  where
    ns = map (\(Definition m x _) -> (m, x)) ds

--------------------------------------------------------
---------------- replying to clients -------------------
--------------------------------------------------------

reply :: RemoteAddress -> O.Packet -> Game ()
reply remote msg = gets sLocal >>= \local -> liftIO $ O.sendTo local msg remote

replyOK :: RemoteAddress -> Game ()
replyOK = flip reply (O.p_message "/ok" [])

replyOKVal :: String -> RemoteAddress -> Game ()
replyOKVal str = flip reply (O.p_message "/ok" [utf8String str])

replyError :: String -> RemoteAddress -> Game ()
replyError err = flip reply (O.p_message "/error" [utf8String err])

-- broadcast to all connected players
broadcast :: O.Packet -> Game ()
broadcast m = do
  ps <- gets sPlayers
  local <- gets sLocal
  mapM_ (liftIO . O.sendTo local m . pAddress) ps

utf8String :: String -> O.Datum
utf8String s = O.AsciiString $ encodeUtf8 $ T.pack s

toUTF8 :: O.Ascii -> String
toUTF8 x = T.unpack $ decodeUtf8 x
