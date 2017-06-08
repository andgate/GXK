{-# LANGUAGE CPP #-}
module Gx.Internal.Backend
  ( module Gx.Internal.Backend.Types
#ifdef WITHGLFW
  , module Gx.Internal.Backend.GLFW
#elif WITHGLUT
  , module Gx.Internal.Backend.GLUT
#endif
  , defaultBackendState
  )
where

import Gx.Internal.Backend.Types

#ifdef WITHGLFW
import Gx.Internal.Backend.GLFW
#elif WITHGLUT
import Gx.Internal.Backend.GLUT
#endif

#ifdef WITHGLFW
defaultBackendState :: GLFWState
#elif WITHGLUT
defaultBackendState :: GLUTState
#else
#error No default backend defined
#endif
defaultBackendState = initBackendState