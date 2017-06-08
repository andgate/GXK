module Gx.Internal.App where

import           Gx.AppConfig
import           Gx.Data.App
import           Gx.Data.Input
import           Gx.Data.Window
import           Gx.Internal.Backend
import           Gx.Internal.Input
import           Gx.Internal.Window

import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.IORef
import           Gx.Data.IORef.Lens
import           Data.Maybe                        (fromMaybe)
import           Data.Yaml
import           Linear
import qualified System.Mem                        as System

playWithBackend :: (Backend b, AppListener w) => b -> w -> IO ()
playWithBackend backend world = do
  backendRef <- newIORef backend
  appRef <- newIORef =<< mkApp world

  appConfig <- loadAppConfig

  let win = appConfigToWindow appConfig
  appRef & appWindow @~ win

  let deltaTime = fromIntegral (appConfigMaxFPS appConfig) ** (-1)
  appRef & appDeltaTime @~ deltaTime

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

  appDraw appRef

  appPostUpdate appRef

  handleAppStatus appRef backendRef

  System.performGC

handleAppStatus :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
handleAppStatus appRef backendRef = do
  shouldQuit <- appRef^@appStatus
  case shouldQuit of
    AppPlay -> return ()
    AppQuit -> exitBackend backendRef

resizeWindow :: (AppListener w, Backend b)
             => AppRef w
             -> IORef b
             -> V2 Int   -- ^ New size of the window
             -> IO ()
resizeWindow appRef _ wS =
  appResize appRef wS

pauseApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
pauseApplication app _ =
  appPause app

resumeApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
resumeApplication app _ =
  appResume app

disposeApplication :: (AppListener w, Backend b) => AppRef w -> IORef b -> IO ()
disposeApplication app _ =
  appDispose app
