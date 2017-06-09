{-# LANGUAGE TemplateHaskell
  #-}
module Gx.Internal.Data.Input where

import Gx.Data.Input

import Control.Lens
import Data.Map.Lazy (Map)
import Linear

import qualified Data.Map.Lazy as Map

type InputEventMap k  = Map k InputEvent
type KeyMap      = InputEventMap Key
type ButtonMap   = InputEventMap MouseButton

data InputState
  = InputState
  { _inputKeyTable :: KeyMap
  , _inputButtonTable :: ButtonMap
  , _inputMousePos1 :: V2 Double
  , _inputMousePos2 :: V2 Double
  }

mkInputState :: InputState
mkInputState = 
  InputState
  { _inputKeyTable = Map.empty
  , _inputButtonTable = Map.empty
  , _inputMousePos1 = zero
  , _inputMousePos2 = zero
  }


makeLenses ''InputState