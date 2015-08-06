module GXK.App where

import GXK.Data.App
import GXK.Internal.App
import GXK.Internal.Backend

import Control.Concurrent
import Control.Lens
import Data.IORef
import GXK.Data.IORef.Lens


play :: AppListener w => w -> IO ()
play = playWithBackend defaultBackendState

quitApp :: AppRef a -> IO ()
quitApp appRef =
  appRef & appStatus @~ AppQuit

appDelay :: Double -> IO ()
appDelay s = threadDelay ms
  where ms = round $ s * 1000000
