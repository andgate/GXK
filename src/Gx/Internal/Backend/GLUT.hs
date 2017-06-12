{-# LANGUAGE CPP
           , FlexibleInstances
           , MultiParamTypeClasses
 #-}
module Gx.Internal.Backend.GLUT where

#ifdef WITHGLUT

import Gx.Data.App
import Gx.Data.AppConfig
import Gx.Internal.Backend.Types
import Gx.Internal.Input
import Gx.Data.Input (Key (..), MouseButton (..))
import qualified Gx.Data.Input as I

import Control.Lens
import Control.Monad
import Data.IORef
import Linear


import Graphics.UI.GLUT                    (get,($=))
import qualified Graphics.UI.GLUT          as GLUT
import qualified Graphics.Rendering.OpenGL as GL


type GLUTRef w = IORef (GLUTState w)

data GLUTState w =
  GLUTState
  { _glutStatus :: GLUTStatus
  , _glutApp :: AppState w
  }

data GLUTStatus = GLUTPlay | GLUTPause | GLUTExit


class HasGLUTState a w where
  glutState :: Lens' (a w) (GLUTState w)

  glutStatus :: Lens' (a w) GLUTStatus
  glutStatus = glutState . go where go f s = (\v -> s{_glutStatus = v}) <$> f (_glutStatus s)

  glutApp :: Lens' (a w) (AppState w)
  glutApp = glutState . go where go f s = (\v -> s{_glutApp = v}) <$> f (_glutApp s)

instance HasGLUTState GLUTState w where
  glutState = id

instance HasAppState GLUTState w where
  appState = glutApp

mkGLUTState :: AppState w -> GLUTState w
mkGLUTState as =
  GLUTState
    { _glutStatus = GLUTPlay
    , _glutApp = as
    }


inApp :: GLUTRef w -> App w () -> IO ()
inApp ref action = do
  glut <- readIORef ref
  app' <- execApp action (glut^.glutApp)
  modifyIORef ref (glutApp .~ app')


{-
instance Backend GLUTState where
  initBackendState  = initGLUTState
  initializeBackend = initGLUT

  -- non-freeglut doesn't like this: (\_ -> GLUT.leaveMainLoop)
  --exitBackend _ = System.exitWith System.ExitSuccess
  exitBackend = glutAppStatus @~ GLUTExit

  openWindow = openWindowGLUT
  dumpBackendState = dumpStateGLUT

  installCallbacks ref callbacks =
    mapM_ (\f -> f ref callbacks)
      [ installDisplayCallbackGLUT
      , installPauseResumeCallbackGLUT
      , installWindowCloseCallbackGLUT
      , installReshapeCallbackGLUT
      , installKeyboardCallbackGLUT
      , installMouseMoveCallbackGLUT
      , installMouseCallbackGLUT
      ]

  -- Call the GLUT mainloop.
  -- This function will return when something calls GLUT.leaveMainLoop
  runMainLoop _   = GLUT.mainLoop

  getWindowDimensions _ = do
    GL.Size sizeX sizeY <- get GLUT.windowSize
    return . Just $ fromEnum <$> V2 sizeX sizeY

  elapsedTime _ =
    liftM ((/ 1000) . fromIntegral) $ get GLUT.elapsedTime

  sleep _ =
    threadDelay . round . (* 1000000)

-}

playBackend :: AppState w -> IO ()
playBackend as = do
    ref <- newIORef $ mkGLUTState as
    initGLUT False
    openWindowGLUT ref
    installCallbacksGLUT ref

    inApp ref $
      join $ use appCreate

    GLUT.mainLoop


-- Initialise -----------------------------------------------------------------
initGLUT ::  Bool -> IO ()
initGLUT debug = do
  (_progName, _args)  <- GLUT.getArgsAndInitialize

  glutVersion         <- get GLUT.glutVersion
  when debug . putStrLn $ "  glutVersion       = " ++ show glutVersion

  GLUT.initialDisplayMode $= [ GLUT.RGBAMode, GLUT.DoubleBuffered]

  -- See if our requested display mode is possible
  displayMode         <- get GLUT.initialDisplayMode
  displayModePossible <- get GLUT.displayModePossible
  when debug . putStrLn $  "  displayMode       = " ++ show displayMode ++ "\n"
                        ++ "  possible          = " ++ show displayModePossible ++ "\n"




-- Open Window ----------------------------------------------------------------
openWindowGLUT :: GLUTRef w -> IO ()
openWindowGLUT ref = do
  win <- (^.glutApp.appWindow) <$> readIORef ref

  let (x, y) = bimap fromIntegral fromIntegral $ win^.windowPosition
      (w, h) = bimap fromIntegral fromIntegral $ win^.windowSize
  GLUT.initialWindowSize $= GL.Size w h
  GLUT.initialWindowPosition $= GL.Position x y

  GLUT.createWindow $ win^.windowName

  GLUT.windowSize $= GL.Size w h

  case win^.windowState of
    WindowFullscreen -> do
      GLUT.gameModeCapabilities $=
           [ GLUT.Where' GLUT.GameModeWidth GLUT.IsEqualTo $ fromIntegral w
           , GLUT.Where' GLUT.GameModeHeight GLUT.IsEqualTo $ fromIntegral h ]
      void GLUT.enterGameMode
    WindowFloating -> return ()

  --  Switch some things.
  --  auto repeat interferes with key up / key down checks.
  --  BUGS: this doesn't seem to work?
  GLUT.perWindowKeyRepeat   $= GLUT.PerWindowKeyRepeatOff


-- Dump State -----------------------------------------------------------------
dumpStateGLUT
        :: GLUTRef e
        -> IO ()

dumpStateGLUT _
 = do
        wbw             <- get GLUT.windowBorderWidth
        whh             <- get GLUT.windowHeaderHeight
        rgba            <- get GLUT.rgba

        rgbaBD          <- get GLUT.rgbaBufferDepths
        colorBD         <- get GLUT.colorBufferDepth
        depthBD         <- get GLUT.depthBufferDepth
        accumBD         <- get GLUT.accumBufferDepths
        stencilBD       <- get GLUT.stencilBufferDepth

        doubleBuffered  <- get GLUT.doubleBuffered

        colorMask       <- get GLUT.colorMask
        depthMask       <- get GLUT.depthMask

        putStrLn $ "* dumpGlutState\n"
                ++ "  windowBorderWidth  = " ++ show wbw            ++ "\n"
                ++ "  windowHeaderHeight = " ++ show whh            ++ "\n"
                ++ "  rgba               = " ++ show rgba           ++ "\n"
                ++ "  depth      rgba    = " ++ show rgbaBD         ++ "\n"
                ++ "             color   = " ++ show colorBD        ++ "\n"
                ++ "             depth   = " ++ show depthBD        ++ "\n"
                ++ "             accum   = " ++ show accumBD        ++ "\n"
                ++ "             stencil = " ++ show stencilBD      ++ "\n"
                ++ "  doubleBuffered     = " ++ show doubleBuffered ++ "\n"
                ++ "  mask         color = " ++ show colorMask      ++ "\n"
                ++ "               depth = " ++ show depthMask      ++ "\n"

--------------------------------------------------------------------------------
-- | Callbacks

installCallbacksGLUT :: GLUTRef w -> IO ()
installCallbacksGLUT ref = do
  GLUT.displayCallback    $= callbackDisplay ref
  GLUT.crossingCallback   $= Just (callbackVisibility ref)
  GLUT.closeCallback      $= Just (callbackWindowClose ref)
  GLUT.reshapeCallback    $= Just (callbackReshape ref)

  -- Keyboard callbacks
  GLUT.keyboardCallback   $= Just (callbackKeyboard ref Down)
  GLUT.keyboardUpCallback $= Just (callbackKeyboard ref Up)
  GLUT.specialCallback    $= Just (callbackSpecialKey ref Down)
  GLUT.specialUpCallback  $= Just (callbackSpecialKey ref Up)

  -- Mouse callbacks
  GLUT.motionCallback         $= Just (callbackMouseMove ref)
  GLUT.passiveMotionCallback  $= Just (callbackMouseMove ref)
  GLUT.mouseCallback          $= Just (callbackMouse ref)

-- Display callback  -----------------------------------------------------
-- | Callback called every frame while window is active
callbackDisplay :: GLUTRef w -> IO ()
callbackDisplay ref = do
  glut <- readIORef ref

  case glut ^. glutStatus of
    GLUTPause ->
        return ()
    GLUTExit  -> do
        GLUT.leaveGameMode
        maybeWin <- get GLUT.currentWindow
        forM_ maybeWin GLUT.destroyWindow

    GLUTPlay  -> do
        inApp ref $ do
          updateInput
          join $ use appUpdate


        -- swap front and back buffers
        GLUT.swapBuffers

        -- Don't report errors by default.
        -- The windows OpenGL implementation seems to complain for no reason.
        --  GLUT.reportErrors
        GLUT.postRedisplay Nothing

        modifyIORef ref $ \gs ->
          if (gs^.appStatus) == AppQuit
            then gs & glutStatus.~GLUTExit
            else gs

    
-- App Status Callback -----------------------------------------------------
-- | Callback for when the app is paused/resumed.
callbackVisibility :: GLUTRef w -> GLUT.Crossing -> IO ()
callbackVisibility ref vis =
  case vis of
    GLUT.WindowEntered -> do
      modifyIORef ref (glutStatus .~ GLUTPlay)
      inApp ref $ join $ use appResume

    GLUT.WindowLeft -> do
      modifyIORef ref (glutStatus .~ GLUTPause)
      inApp ref $ join $ use appPause


-- Close Callback -------------------------------------------------------------
-- | Callback for when the user closes the window.
--   We can do some cleanup here.
callbackWindowClose :: GLUTRef w -> IO ()
callbackWindowClose ref =
  inApp ref $ join $ use appDispose


-- Reshape Callback -----------------------------------------------------------
callbackReshape :: GLUTRef w -> GLUT.Size -> IO ()
callbackReshape ref (GLUT.Size sizeX sizeY) =
  inApp ref $ do
    resize <- use appResize
    resize (fromEnum <$> V2 sizeX sizeY)


-- Keyboard Callback ----------------------------------------------------------
callbackKeyboard :: GLUTRef w -> InputEvent -> Char -> GLUT.Position -> IO ()
callbackKeyboard ref ev key _ =
  inApp ref $ updateKeyboardInput (fromGLUT key) ev


callbackSpecialKey :: GLUTRef w -> InputEvent -> GLUT.SpecialKey -> GLUT.Position -> IO ()
callbackSpecialKey ref ev key _ =
  inApp ref $ updateKeyboardInput (fromGLUT key) ev


-- Move Movement Callback ------------------------------------------------------------
callbackMouseMove :: GLUTRef w -> GLUT.Position -> IO ()
callbackMouseMove ref (GLUT.Position posX posY) =
  inApp ref $ updateMouseMoveInput (fromIntegral <$> V2 posX posY)


-- Mouse Callback ------------------------------------------------------
callbackMouse :: GLUTRef w -> GLUT.MouseButton -> GLUT.KeyState -> GLUT.Position -> IO ()
callbackMouse ref button ev (GLUT.Position posX posY) =
  case button of
    GLUT.WheelUp ->
      inApp ref $ updateScrolledInput (V2 0 1)
    
    GLUT.WheelDown ->
      inApp ref $ updateScrolledInput (V2 0 (-1))
    
    _ ->
      inApp ref $ updateMouseClickInput button' ev' (V2 posX' posY')
  where
    button' = fromGLUT button
    ev' = fromGLUT ev
    posX' = fromIntegral posX
    posY' = fromIntegral posY


-- GLUT type Conversion -------------------------------------------------------
class GLUTConv a b where
  fromGLUT :: a -> b

instance GLUTConv Char Key where
  fromGLUT c =
    case c of
      '\32'  -> Key'Space
      '\13'  -> Key'Enter
      '\9'   -> Key'Tab
      '\ESC' -> Key'Escape
      '\DEL' -> Key'Delete
      'A'    -> Key'A
      'a'    -> Key'a
      'B'    -> Key'B
      'b'    -> Key'b
      'C'    -> Key'C
      'c'    -> Key'c
      'D'    -> Key'D
      'd'    -> Key'd
      'E'    -> Key'E
      'e'    -> Key'e
      'F'    -> Key'F
      'f'    -> Key'f
      'G'    -> Key'G
      'g'    -> Key'g
      'H'    -> Key'H
      'h'    -> Key'h
      'I'    -> Key'I
      'i'    -> Key'i
      'J'    -> Key'J
      'j'    -> Key'j
      'K'    -> Key'K
      'k'    -> Key'k
      'L'    -> Key'L
      'l'    -> Key'l
      'M'    -> Key'M
      'm'    -> Key'm
      'N'    -> Key'N
      'n'    -> Key'n
      'O'    -> Key'O
      'o'    -> Key'o
      'P'    -> Key'P
      'p'    -> Key'p
      'Q'    -> Key'Q
      'q'    -> Key'q
      'R'    -> Key'R
      'r'    -> Key'r
      'S'    -> Key'S
      's'    -> Key's
      'T'    -> Key'T
      't'    -> Key't
      'U'    -> Key'U
      'u'    -> Key'u
      'V'    -> Key'V
      'v'    -> Key'v
      'W'    -> Key'W
      'w'    -> Key'w
      'X'    -> Key'X
      'x'    -> Key'x
      'Y'    -> Key'Y
      'y'    -> Key'y
      'Z'    -> Key'Z
      'z'    -> Key'z
      '`'    -> Key'GraveAccent
      '1'    -> Key'1
      '2'    -> Key'2
      '3'    -> Key'3
      '4'    -> Key'4
      '5'    -> Key'5
      '6'    -> Key'6
      '7'    -> Key'7
      '8'    -> Key'8
      '9'    -> Key'9
      '0'    -> Key'0
      '-'    -> Key'Minus
      '='    -> Key'Equal
      '~'    -> Key'Tilde
      '!'    -> Key'Exclaim
      '@'    -> Key'Ampersand
      '#'    -> Key'Hash
      '$'    -> Key'Dollar
      '%'    -> Key'Percent
      '^'    -> Key'Caret
      '&'    -> Key'Ampersand
      '*'    -> Key'Asterisk
      '('    -> Key'LeftParens
      ')'    -> Key'RightParens
      '_'    -> Key'Underscore
      '+'    -> Key'Plus
      '['    -> Key'LeftBracket
      ']'    -> Key'RightBracket
      '\\'   -> Key'Backslash
      ';'    -> Key'Semicolon
      '\''   -> Key'Apostrophe
      ','    -> Key'Comma
      '.'    -> Key'Period
      '/'    -> Key'Slash
      '{'    -> Key'LeftCurlyBracket
      '}'    -> Key'RightCurlyBracket
      '|'    -> Key'Pipe
      ':'    -> Key'Colon
      '"'    -> Key'Quote
      '<'    -> Key'LesserThan
      '>'    -> Key'GreaterThan
      '?'    -> Key'Question
      _      -> Key'Unknown

instance GLUTConv GLUT.SpecialKey Key where
  fromGLUT key =
    case key of
      GLUT.KeyUnknown _          -> Key'Unknown
      GLUT.KeyF1                 -> Key'F1
      GLUT.KeyF2                 -> Key'F2
      GLUT.KeyF3                 -> Key'F3
      GLUT.KeyF4                 -> Key'F4
      GLUT.KeyF5                 -> Key'F5
      GLUT.KeyF6                 -> Key'F6
      GLUT.KeyF7                 -> Key'F7
      GLUT.KeyF8                 -> Key'F8
      GLUT.KeyF9                 -> Key'F9
      GLUT.KeyF10                -> Key'F10
      GLUT.KeyF11                -> Key'F11
      GLUT.KeyF12                -> Key'F12
      GLUT.KeyLeft               -> Key'Left
      GLUT.KeyUp                 -> Key'Up
      GLUT.KeyRight              -> Key'Right
      GLUT.KeyDown               -> Key'Down
      GLUT.KeyPageUp             -> Key'PageUp
      GLUT.KeyPageDown           -> Key'PageDown
      GLUT.KeyHome               -> Key'Home
      GLUT.KeyEnd                -> Key'End
      GLUT.KeyInsert             -> Key'Insert
      GLUT.KeyNumLock            -> Key'NumLock
      GLUT.KeyBegin              -> Key'RightSuper
      GLUT.KeyDelete             -> Key'Delete
      GLUT.KeyShiftL             -> Key'LeftShift
      GLUT.KeyShiftR             -> Key'RightShift
      GLUT.KeyCtrlL              -> Key'LeftControl
      GLUT.KeyCtrlR              -> Key'RightControl
      GLUT.KeyAltL               -> Key'LeftAlt
      GLUT.KeyAltR               -> Key'RightAlt

instance GLUTConv GLUT.MouseButton MouseButton where
  fromGLUT button =
    case button of
      GLUT.LeftButton           -> Left'Button
      GLUT.MiddleButton         -> Middle'Button
      GLUT.RightButton          -> Right'Button
      GLUT.AdditionalButton i   -> Additional'Button i
      _                         -> Unknown'Button

-- | Convert GLUTs key states to our internal ones.
instance GLUTConv GLUT.KeyState InputEvent where
  fromGLUT state =
    case state of
      GLUT.Down       -> Down
      GLUT.Up         -> Up

 -- WITHGLUT
#endif