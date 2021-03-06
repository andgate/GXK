{-# LANGUAGE TemplateHaskell #-}
module Gx.Data.Window where

import Control.Lens

data WindowState = WindowFloating | WindowFullscreen
  deriving (Show)

data Window = Window
  { _windowName :: String
  , _windowPosition :: (Int, Int)
  , _windowSize :: (Int, Int)
  , _windowState :: WindowState
  }

defWindow :: Window
defWindow =
  Window
  { _windowName = "default"
  , _windowPosition = (0, 0)
  , _windowSize = (640, 480)
  , _windowState = WindowFloating
  }

makeClassy ''Window
