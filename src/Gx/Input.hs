{-# LANGUAGE ExistentialQuantification #-}
module Gx.Input
  ( module Gx.Input
  , module Gx.Data.Input
  )
where

import Gx.Data.App
import Gx.Data.Input

import Gx.Internal.Data.Input

import Control.Lens
import Data.IORef
import Gx.Data.IORef.Lens

registerInputListener :: InputListener l => AppRef w -> l -> IO ()
registerInputListener appRef listener = do
  let reg = register listener
  appRef & appInput.inputListeners @%~ (reg:)
