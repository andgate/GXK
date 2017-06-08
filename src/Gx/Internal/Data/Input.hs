{-# LANGUAGE ExistentialQuantification
           , TemplateHaskell
  #-}
module Gx.Internal.Data.Input where

import Gx.Data.Input

import Control.Lens
import qualified Data.HashTable.IO as H
import Linear

data Registrable = forall a . InputListener a => MkInputListener a

register :: InputListener a => a -> Registrable
register = MkInputListener

type InputTable k  = H.BasicHashTable k InputState
type KeyTable      = InputTable Key
type ButtonTable   = InputTable MouseButton

data Input = Input
  { _inputKeyTable :: KeyTable
  , _inputButtonTable :: ButtonTable
  , _inputListeners :: [Registrable]
  , _inputMousePos1 :: V2 Double
  , _inputMousePos2 :: V2 Double
  }

makeLenses ''Input

mkInput :: IO Input
mkInput = do
  keyTable <- H.new
  buttonTable <- H.new
  let listeners = []
      mousePos = zero
  return
    Input
    { _inputKeyTable = keyTable
    , _inputButtonTable = buttonTable
    , _inputListeners = listeners
    , _inputMousePos1 = mousePos
    , _inputMousePos2 = mousePos
    }
