module Gx.Internal.Window where

import Gx.Data.App
import Gx.Internal.Backend.Types

import Control.Monad
import Data.IORef
import Gx.Data.IORef.Lens

createWindow :: (Backend b, AppListener w) => AppRef w -> IORef b -> Callbacks -> IO ()
createWindow appRef backendRef callbacks = do
  let debug = True
  initializeBackend backendRef debug
  openWindow backendRef =<< appRef ^@ appWindow
  installCallbacks backendRef callbacks

  -- Dump some debugging info
  when debug $ do
    dumpBackendState backendRef
    -- Not implemented
    --dumpFramebufferState
    --dumpFragmentState

  when debug . putStrLn $ "* entering mainloop.."

  appCreate appRef
  runMainLoop backendRef

  when debug . putStrLn $ "* all done"
