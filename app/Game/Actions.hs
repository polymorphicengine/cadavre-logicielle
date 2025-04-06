module Game.Actions where

import Control.Concurrent.MVar (putMVar, takeMVar)
import Control.Monad (unless, void)
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Text as T (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Game.Hint
import Game.Types
import Game.UI
import Graphics.UI.Threepenny as UI (Element, UI, askWindow, element, getElementById, liftUI, set, text, (#))
import Sound.Osc.Fd as O
import qualified Sound.Osc.Transport.Fd.Udp as O

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

addDefinition :: String -> String -> RemoteAddress -> Game ()
addDefinition name code remote = do
  may <- interpretDefinition name code

  case may of
    Right e -> replyError e remote
    Left (first, typ) -> do
      replyOK remote
      pname <- getNameFromAddress remote
      if first
        then do
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

setStream :: String -> String -> Game (Either () String)
setStream name code = do
  response <- evaluateStatement (generateStreamCode name code)
  case response of
    Left _ -> return $ Left ()
    Right e -> return $ Right e

getType :: String -> String -> Game (Either (Bool, String) String)
getType name code = do
  mayt <- Game.Actions.interpretType code
  ds <- gets sDefinitions
  case lookup name (map (\x@(Definition n _ _) -> (n, x)) ds) of
    Just d -> case mayt of
      Left t -> if dType d == t then return $ Left (False, t) else return $ Right "Cannot change the type of definitions!"
      Right e -> return $ Right e
    Nothing -> case mayt of
      Left x -> return $ Left (True, x)
      Right y -> return $ Right y

defineVariable :: String -> String -> Game (Either () String)
defineVariable typ name = do
  response <- evaluateStatement (generateDefCode typ name)
  case response of
    Left _ -> return $ Left ()
    Right e -> return $ Right e

-- returns error or true for the first time defining, false otherwise
interpretDefinition :: String -> String -> Game (Either (Bool, String) String)
interpretDefinition name code = do
  mayt <- getType name code
  case mayt of
    Left (False, t) -> do
      resp <- setStream name code
      case resp of
        Left _ -> return $ Left (False, t) -- successfully changed the definition
        Right e -> return $ Right e
    Left (True, t) -> do
      resp <- setStream name code
      case resp of
        Left _ -> do
          resp2 <- defineVariable t name
          case resp2 of
            Left _ -> return $ Left (True, t) -- first definition
            Right e -> return $ Right e
        Right e -> return $ Right e
    Right e -> return $ Right e

generateStreamCode :: String -> String -> String
generateStreamCode name code = "streamSet tidal " ++ show name ++ " $ " ++ code

generateDefCode :: String -> String -> String
generateDefCode typ name = "let " ++ name ++ " = _define " ++ show name ++ " :: " ++ typ

--------------------------------------------------------
-------------------- code actions ----------------------
--------------------------------------------------------

-- returns left on success
evaluateStatement :: String -> Game (Either (Maybe String) String)
evaluateStatement stat = do
  hM <- gets sHintMessage
  hR <- gets sHintResponse
  liftIO $ putMVar hM (MStat stat)
  response <- liftIO $ takeMVar hR
  case response of
    RStat Nothing -> return (Left Nothing)
    RStat (Just v) -> return (Left $ Just v)
    RError e -> return (Right e)
    _ -> return $ Right "unkown hint error"

updateCode :: String -> RemoteAddress -> Game ()
updateCode code add = do
  mayp <- getPlayerFromAddress add
  case mayp of
    Nothing -> return ()
    Just p -> do
      el <- liftUI $ getCodeElement p
      void $ liftUI $ element el # set text code

interpretType :: String -> Game (Either String String)
interpretType typ = do
  hM <- gets sHintMessage
  hR <- gets sHintResponse
  liftIO $ putMVar hM (MType typ)
  response <- liftIO $ takeMVar hR
  case response of
    RType t -> return (Left t)
    RError e -> return (Right e)
    _ -> return (Right "unkown hint error")

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
