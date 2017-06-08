module Gx.App where

import Gx.Data.App
import Gx.Internal.App
import Gx.Internal.Backend

import Control.Concurrent
import Control.Lens
import Data.IORef
import Gx.Data.IORef.Lens


play :: AppListener w => w -> IO ()
play = playWithBackend defaultBackendState

quitApp :: AppRef a -> IO ()
quitApp appRef =
  appRef & appStatus @~ AppQuit

appDelay :: Double -> IO ()
appDelay s = threadDelay ms
  where ms = round $ s * 1000000

getDeltaTime :: AppRef a -> IO Double
getDeltaTime appRef =
  appRef ^@ appDeltaTime
