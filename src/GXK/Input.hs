{-# LANGUAGE ExistentialQuantification #-}
module GXK.Input
  ( module GXK.Input
  , module GXK.Data.Input
  )
where

import GXK.Data.App
import GXK.Data.Input

import GXK.Internal.Data.Input

import Control.Lens
import Data.IORef
import GXK.Data.IORef.Lens

registerInputListener :: InputListener l => AppRef w -> l -> IO ()
registerInputListener appRef listener = do
  let reg = register listener
  appRef & appInput.inputListeners @%~ (reg:)
