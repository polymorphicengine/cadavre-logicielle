{-# LANGUAGE FlexibleInstances #-}

module Game.Game where

import Control.Concurrent.MVar
import Control.Monad (unless, void, when)
import Control.Monad.State (StateT, get, gets, lift, modify, runStateT)
import Data.Maybe (fromMaybe)
import Game.Hint
import Game.UI
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (get)
import qualified Network.Socket as N
import Sound.Osc.Fd as O
import Sound.Tidal.Context (Stream)

type RemoteAddress = N.SockAddr

-- a player has a name and an address
data Player
  = Player
  { pName :: String,
    pAddress :: RemoteAddress,
    pCode :: String
  }

instance Eq Player where
  (==) p1 p2 = pName p1 == pName p2

instance Show Player where
  show = pName

-- a definition consists of a name, type, code and def
data Definition
  = Definition
  { dName :: String,
    dType :: String,
    dCode :: String,
    dDef :: String
  }

instance Show Definition where
  show (Definition n t _ _) = n ++ " :: " ++ t

type Definitions = [Definition]

type Players = [Player]

-- state that the playing table contains and acts on
data State
  = State
  { sLocal :: Udp,
    sPlayers :: Players,
    sDefinitions :: Definitions,
    sHintMessage :: MVar InterpreterMessage,
    sHintResponse :: MVar InterpreterResponse,
    sStream :: Stream
  }

type Game = StateT State UI

instance MonadUI Game where
  liftUI = lift . liftUI

runGame :: State -> Game a -> UI (a, State)
runGame st g = runStateT g st

getPlayerFromAddress :: RemoteAddress -> Game (Maybe Player)
getPlayerFromAddress add = do
  ps <- gets sPlayers
  return $ lookup add (map (\p@(Player n a _) -> (a, p)) ps)

getNameFromAddress :: RemoteAddress -> Game String
getNameFromAddress = fmap (maybe "unkown player" pName) . getPlayerFromAddress

getTypeFromName :: Definitions -> String -> String
getTypeFromName ds n = fromMaybe "unkown type" (lookup n ns)
  where
    ns = map (\(Definition m x _ _) -> (m, x)) ds

mkPlayer :: Player -> UI Element
mkPlayer p = UI.p #. "player" #@ playerID p # set UI.text (pName p)

mkCode :: Player -> UI Element
mkCode p = UI.pre #. "code" #@ codeID p # set UI.text (pCode p)

codeID :: Player -> String
codeID p = "code-" ++ pName p

playerID :: Player -> String
playerID p = "player-" ++ pName p

playerWrapper :: Player -> UI Element
playerWrapper p = UI.div #+ [mkPlayer p, mkCode p] #. "player-wrapper"

mkDefinition :: Definition -> UI Element
mkDefinition d = UI.p #. "definition" #@ defID d # set UI.text (show d)

defID :: Definition -> String
defID d = "def-" ++ dName d

addPlayer :: Player -> Game ()
addPlayer p = do
  ps <- gets sPlayers
  unless
    (pName p `elem` map pName ps)
    ( do
        el <- liftUI $ playerWrapper p
        liftUI $ addMessage (pName p ++ " joined the table!")
        liftUI $ addElement "player" "player-container" el
        modify $ \st -> st {sPlayers = p : sPlayers st}
    )

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
          liftUI $ addElement "definition" "definition-container" el
          interpretDefinition True d remote
          modify $ \st -> st {sDefinitions = d : sDefinitions st}
      )

reply :: RemoteAddress -> O.Packet -> Game ()
reply remote msg = gets sLocal >>= \local -> liftIO $ O.sendTo local msg remote

replyOK :: RemoteAddress -> Game ()
replyOK = flip reply (O.p_message "/ok" [])

replyOKVal :: String -> RemoteAddress -> Game ()
replyOKVal str = flip reply (O.p_message "/ok" [O.string str])

replyError :: String -> RemoteAddress -> Game ()
replyError err = flip reply (O.p_message "/error" [O.string err])

-- returns true for success, false otherwise
interpretDefinition :: Bool -> Definition -> RemoteAddress -> Game ()
interpretDefinition first def remote = do
  hM <- gets sHintMessage
  hR <- gets sHintResponse
  liftIO $ putMVar hM (MStat $ dCode def)
  response <- liftIO $ takeMVar hR
  case response of
    RStat Nothing ->
      ( if first
          then
            ( do
                liftIO $ putMVar hM (MStat $ dDef def)
                response2 <- liftIO $ takeMVar hR
                case response2 of
                  RStat Nothing -> replyOK remote
                  RError e -> replyError e remote
                  _ -> replyError "unkown hint error" remote
            )
          else do
            name <- getNameFromAddress remote
            liftUI $ addMessage (name ++ " changed the definition of " ++ dName def)
            replyOK remote
      )
    RError e -> replyError e remote
    _ -> replyError "unkown hint error" remote

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

getType :: String -> RemoteAddress -> Game ()
getType typ remote = do
  hM <- gets sHintMessage
  hR <- gets sHintResponse
  liftIO $ putMVar hM (MType typ)
  response <- liftIO $ takeMVar hR
  case response of
    RType t -> replyOKVal (show t) remote
    RError e -> replyError e remote
    _ -> replyError "unkown hint error" remote

updateCode :: String -> RemoteAddress -> Game ()
updateCode code add = do
  mayp <- getPlayerFromAddress add
  case mayp of
    Nothing -> return ()
    Just p -> do
      el <- liftUI $ getCodeElement p
      void $ liftUI $ element el # set UI.text code

getCodeElement :: Player -> UI Element
getCodeElement p = do
  win <- askWindow
  elMay <- getElementById win (codeID p)
  case elMay of
    Nothing -> playerWrapper p
    Just el -> return el
