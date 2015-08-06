module GXK.Internal.App where

import           GXK.AppConfig
import           GXK.Data.App
import           GXK.Data.Input
import           GXK.Data.Window
import           GXK.Graphics
import           GXK.Graphics.Picture
import           GXK.Internal.Backend
import           GXK.Internal.Input
import           GXK.Internal.Window

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.IORef
import           GXK.Data.IORef.Lens
import           Data.Maybe                        (fromMaybe)
import           Data.Yaml
import qualified System.Mem                        as System

playWithBackend :: (Backend b, AppListener w) => b -> w -> IO ()
playWithBackend backend world = do
  backendRef <- newIORef backend
  appRef <- newIORef =<< mkApp world

  appConfig <- loadAppConfig

  let win = appConfigToWindow appConfig
  appRef & appWindow @~ win

  let callbacks =
        Callbacks
        { displayCallback     = displayUpdate appRef
        , pauseCallback       = pauseApplication appRef
        , resumeCallback      = resumeApplication appRef
        , closeCallback       = disposeApplication appRef
        , reshapeCallback     = resizeWindow appRef
        , keyboardCallback    = updateKeyboardInput appRef
        , mouseMoveCallback   = updateMouseMoveInput appRef
        , mouseButtonCallback = updateMouseClickInput appRef
        , scrollCallback      = updateScrolledInput appRef
        }

  createWindow appRef backendRef callbacks

displayUpdate :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
displayUpdate appRef backendRef = do
  updateInput appRef backendRef
  appUpdate appRef

  pic <- appDraw appRef
  texCache <- appRef ^@ appGfx.gfxTexCache
  winSize <- appRef ^@ appWindow.windowSize

  renderStart
  displayPicture texCache pic winSize
  renderEnd

  appPostUpdate appRef

  handleAppStatus appRef backendRef

  System.performGC

handleAppStatus :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
handleAppStatus appRef backendRef = do
  shouldQuit <- appRef^@appStatus
  case shouldQuit of
    AppPlay -> return ()
    AppQuit -> exitBackend backendRef

resizeWindow :: (AppListener w, Backend b) => AppRef w -> IORef b -> Int -> Int -> IO ()
resizeWindow app _ =
  appResize app

pauseApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
pauseApplication app _ =
  appPause app

resumeApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
resumeApplication app _ =
  appResume app

disposeApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
disposeApplication app _ =
  appDispose app
