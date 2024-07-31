module Editor.Frontend where

{-
    Frontend.hs - defines the html dom for the editor interface
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
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core as C hiding (text)
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)

frontend :: Window -> UI ()
frontend win = do
  void $ return win # set title "cadavre logicielle"

  UI.addStyleSheet win "cadavre-logicielle.css"

  setCallBufferMode NoBuffering

  body <- UI.getBody win # set UI.style [("background-color", "black")]

  void $ element body #+ [UI.div #+ [messageContainer "10:00" "😂", messageContainer "10:01" "lol"]]

messageContainer :: String -> String -> UI Element
messageContainer time message = UI.div #+ [UI.span #. "message-time" # set UI.text time, UI.p #. "message" # set UI.text message] #. "message-container"
