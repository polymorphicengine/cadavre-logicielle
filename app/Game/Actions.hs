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
import qualified Sound.Osc as O

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
            modify $ \st -> st {sPlayers = p : sPlayers st}
            shareDefinitions p
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
      modify $ \st -> st {sPlayers = map (\q -> if pAddress q == add then q {pName = new} else q) $ sPlayers st}

updateCode :: String -> RemoteAddress -> Game ()
updateCode code add = do
  mayp <- getPlayerFromAddress add
  case mayp of
    Nothing -> return ()
    Just p -> do
      el <- liftUI $ getCodeElement p
      void $ liftUI $ element el # set text code

shareDefinitions :: Player -> Game ()
shareDefinitions p = do
  ds <- gets sDefinitions
  mapM_ (\d -> reply (pAddress p) (O.p_message "/define" [utf8String (pName p), utf8String $ dName d, utf8String $ dType d])) ds

--------------------------------------------------------
----------------- definition actions -------------------
--------------------------------------------------------

addDefinition :: Definition -> RemoteAddress -> Game ()
addDefinition d remote = do
  ds <- gets sDefinitions
  name <- getNameFromAddress remote
  if dName d `elem` map dName ds
    then
      unless
        -- already defined variables cannot change type
        (dType d /= getTypeFromName ds (dName d))
        (interpretDefinition False d remote)
    else
      ( do
          el <- liftUI $ mkDefinition d
          liftUI $ addMessage (name ++ " folded the document and revealed " ++ show d)
          broadcast (O.p_message "/define" [utf8String name, utf8String $ dName d, utf8String $ dType d])
          liftUI $ addElement "definition" "definition-container" el
          interpretDefinition True d remote
          modify $ \st -> st {sDefinitions = d : sDefinitions st}
      )

interpretDefinition :: Bool -> Definition -> RemoteAddress -> Game ()
interpretDefinition first def remote = do
  hM <- gets sHintMessage
  hR <- gets sHintResponse
  liftIO $ putMVar hM (MStat $ dCode def)
  response <- liftIO $ takeMVar hR
  case response of
    RStat Nothing ->
      ( if first
          then do
            liftIO $ putMVar hM (MStat $ dDef def)
            response2 <- liftIO $ takeMVar hR
            case response2 of
              RStat Nothing -> replyOK remote
              RError e -> replyError e remote
              _ -> replyError "unkown hint error" remote
          else do
            name <- getNameFromAddress remote
            liftUI $ addMessage (name ++ " changed the definition of " ++ dName def)
            broadcast (O.p_message "/change" [utf8String name, utf8String (dName def)])
            replyOK remote
      )
    RError e -> replyError e remote
    _ -> replyError "unkown hint error" remote

--------------------------------------------------------
-------------------- code actions ----------------------
--------------------------------------------------------

evaluateStatement :: String -> RemoteAddress -> Game ()
evaluateStatement stat remote = do
  updateCode stat remote
  hM <- gets sHintMessage
  hR <- gets sHintResponse
  liftIO $ putMVar hM (MStat stat)
  response <- liftIO $ takeMVar hR
  case response of
    RStat Nothing -> replyOK remote
    RStat (Just v) -> replyOKVal (show v) remote
    RError e -> replyError e remote
    _ -> replyError "unkown hint error" remote

typeAction :: String -> RemoteAddress -> Game ()
typeAction typ remote = do
  hM <- gets sHintMessage
  hR <- gets sHintResponse
  liftIO $ putMVar hM (MType typ)
  response <- liftIO $ takeMVar hR
  case response of
    RType t -> replyOKVal (show t) remote
    RError e -> replyError e remote
    _ -> replyError "unkown hint error" remote

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
  return $ lookup add (map (\p@(Player n a _ _) -> (a, p)) ps)

getNameFromAddress :: RemoteAddress -> Game String
getNameFromAddress = fmap (maybe "unkown player" pName) . getPlayerFromAddress

getTypeFromName :: Definitions -> String -> String
getTypeFromName ds n = fromMaybe "unkown type" (lookup n ns)
  where
    ns = map (\(Definition m x _ _) -> (m, x)) ds

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
