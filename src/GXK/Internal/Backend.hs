{-# LANGUAGE CPP #-}
module GXK.Internal.Backend
  ( module GXK.Internal.Backend.Types
#ifdef WITHGLFW
  , module GXK.Internal.Backend.GLFW
#elif WITHGLUT
  , module GXK.Internal.Backend.GLUT
#endif
  , defaultBackendState
  )
where

import GXK.Internal.Backend.Types

#ifdef WITHGLFW
import GXK.Internal.Backend.GLFW
#elif WITHGLUT
import GXK.Internal.Backend.GLUT
#endif

#ifdef WITHGLFW
defaultBackendState :: GLFWState
#elif WITHGLUT
defaultBackendState :: GLUTState
#else
#error No default backend defined
#endif
defaultBackendState = initBackendState
