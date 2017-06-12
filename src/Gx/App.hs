module Gx.App where


import Gx.Data.App
import Gx.Data.Input
import Gx.Data.Window
import Gx.Internal.Backend

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent
import Control.Lens


play :: w -> AppLifeCycle w -> InputListener w -> Window -> IO ()
play world lifecycle input win
  = playBackend (mkAppState world lifecycle input win)

quitApp :: App w ()
quitApp =
  appStatus .= AppQuit

appDelay :: Double -> App w ()
appDelay s = liftIO $ threadDelay ms
  where ms = round $ s * 1000000
