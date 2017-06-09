module Gx.Internal.Window where

import Gx.Data.App
import Gx.Internal.Backend.Types

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef


createWindow :: Backend b => IORef b -> Callbacks -> App ()
createWindow backendRef callbacks = do
  let debug = True
  win <- use appWindow
  liftIO $ do
    initializeBackend backendRef debug
    openWindow backendRef win
    installCallbacks backendRef callbacks

    -- Dump some debugging info
    when debug $
      dumpBackendState backendRef
      -- Not implemented
      --dumpFramebufferState
      --dumpFragmentState

    when debug . putStrLn $ "* entering mainloop.."

  join $ use $ appListener . appCreate

  liftIO $ do
    runMainLoop backendRef
    when debug . putStrLn $ "* all done"
