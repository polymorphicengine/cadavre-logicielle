module Game.CommandLine where

{-
    CommandLine.hs - command line interface for the editor
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

import Options.Applicative

data Config = Config
  { tpPort :: Int
  }
  deriving (Eq, Show)

conf :: ParserInfo Config
conf =
  info
    (configParser <**> helper)
    ( fullDesc
        <> progDesc "An interactive interpreter for zwirn"
        <> header "zwirn"
    )

configParser :: Parser Config
configParser = Config <$> tpPortParser

tpPortParser :: Parser Int
tpPortParser =
  option
    auto
    ( long "tp-port"
        <> short 'p'
        <> help "Specify the threepenny port"
        <> showDefault
        <> value 8023
        <> metavar "INT"
    )
