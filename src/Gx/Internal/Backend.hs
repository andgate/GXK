{-# LANGUAGE CPP #-}
module Gx.Internal.Backend
  ( module Gx.Internal.Backend.Types
  , playBackend
  )
where

import Gx.Internal.Backend.Types

#ifdef WITHGLFW
import Gx.Internal.Backend.GLFW (playBackend)
#elif WITHGLUT
import Gx.Internal.Backend.GLUT (playBackend)
#else
#error No default backend defined
#endif