module Main where

import Control.Lens
import Gx

data Player = 
  Player
  { _playerX :: Int 
  , _playerY :: Int
  }

defPlayer = Player 0 0

main :: IO ()
main = play defPlayer defAppLifeCycle defInputListener defWindow