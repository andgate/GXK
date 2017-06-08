{-# LANGUAGE Rank2Types
           , TemplateHaskell
  #-}
module Gx.Data.App where

import Control.Lens
import Data.IORef
import Linear

import Gx.Internal.Data.Input (mkInput, Input)
import Gx.Data.Window

type AppRef w = IORef (App w)

getApp :: AppListener w => AppRef w -> IO (App w)
getApp = readIORef

class AppListener w where
  appCreate :: AppRef w -> IO ()
  appCreate _ = return ()

  appResize :: AppRef w -> V2 Int -> IO ()
  appResize _ winSize = return ()

  appUpdate :: AppRef w -> IO ()
  appUpdate _ = return ()

  appDraw :: AppRef w -> IO ()
  appDraw _ = return ()

  appPostUpdate :: AppRef w -> IO ()
  appPostUpdate _ = return ()

  appPause :: AppRef w -> IO ()
  appPause _ = return ()

  appResume :: AppRef w -> IO ()
  appResume _ = return ()

  appDispose :: AppRef w -> IO ()
  appDispose _ = return ()

data App w = App
  { _appInput :: Input
  , _appWindow :: Window
  , _appWorld :: w
  , _appStatus :: AppStatus
  , _appDeltaTime :: Double
  }

data AppStatus = AppPlay | AppQuit

makeLenses ''App

mkApp :: AppListener w => w -> IO (App w)
mkApp world = do
  input     <- mkInput
  let win = windowDefault
      status = AppPlay
      deltaTime = 0

  return
    App
    { _appInput = input
    , _appWindow = win
    , _appWorld = world
    , _appStatus = status
    , _appDeltaTime = deltaTime
    }
