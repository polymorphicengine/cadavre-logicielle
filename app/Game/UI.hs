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
import Data.Time
import Game.Types
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (get, text, value)

mkName :: Player -> UI Element
mkName p = UI.pre #. "name" #@ nameID p # set UI.text (pName p)

mkCode :: Player -> UI Element
mkCode p = UI.pre #. "code" #@ codeID p # set UI.text (pCode p)

codeID :: Player -> String
codeID p = "code-" ++ pName p

nameID :: Player -> String
nameID p = "player-" ++ pName p

mkPlayer :: Player -> UI Element
mkPlayer p = UI.div #+ [mkName p, mkCode p] #. "player"

mkDefinition :: Definition -> UI Element
mkDefinition d = UI.pre #. "definition" #@ defID d # set UI.text (show d)

defID :: Definition -> String
defID d = "def-" ++ dName d

mkMessage :: String -> String -> UI Element
mkMessage t m = UI.pre # set UI.text (t ++ " - " ++ m) #. "message"

addMessage :: String -> UI ()
addMessage m = do
  t <- liftIO getZonedTime
  el <- mkMessage (showTime t) m
  addElement "message" "message-container" el

showTime :: ZonedTime -> String
showTime = take 8 . show . localTimeOfDay . zonedTimeToLocalTime

addElement :: String -> String -> Element -> UI ()
addElement className containerId el = do
  win <- askWindow
  els <- getElementsByClassName win className
  mayContainer <- getElementById win containerId
  case mayContainer of
    Nothing -> return ()
    Just cont -> void $ element cont # set UI.children (el : els)

infixl 8 #@

(#@) :: UI Element -> String -> UI Element
(#@) mx s = mx # set (attr "id") s
