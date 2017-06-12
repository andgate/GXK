{-# LANGUAGE RankNTypes
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , FlexibleInstances
  #-}
module Gx.Data.App where

import Control.Lens
import Control.Monad.State
import Linear

import Gx.Data.Input
import Gx.Data.Window


newtype App a r = App { unApp :: StateT (AppState a) IO r }
    deriving (Functor, Applicative, Monad, MonadState (AppState a), MonadIO)

runApp :: App a r -> AppState a -> IO (r, AppState a)
runApp a = runStateT (unApp a)

evalApp :: App a r -> AppState a -> IO r
evalApp a = evalStateT (unApp a)

execApp :: App a r -> AppState a -> IO (AppState a)
execApp a = execStateT (unApp a)

-------------------------------------------------------------------------------
-- App State ------------------------------------------------------------------

data AppState a = AppState
  { _appData     :: a
  , _appCycle     :: AppLifeCycle a
  , _appInputListener :: InputListener a
  , _appInput     :: InputState
  , _appWindow    :: Window
  , _appStatus    :: AppStatus
  , _appDeltaTime :: Double
  }

data AppStatus = AppPlay | AppQuit
  deriving (Eq, Ord, Enum)


instance HasAppLifeCycle AppState a where
  appLifeCycle = appCycle

instance HasInputListener AppState a where
  inputListener = appInputListener

instance HasInputState (AppState a) where
  inputState = appInput

instance HasWindow (AppState a) where
  window = appWindow



mkAppState :: a -> AppLifeCycle a -> InputListener a -> Window -> AppState a
mkAppState a cy l win =
  AppState
    { _appData = a
    , _appCycle = cy
    , _appInput = mkInputState
    , _appInputListener = l
    , _appWindow = win
    , _appStatus = AppPlay
    , _appDeltaTime = 0
    }

class HasAppState m a where
  appState :: Lens' (m a) (AppState a)

  appData :: Lens' (m a) a
  appData = appState . go where go f s = (\v -> s{_appData = v}) <$> f (_appData s)

  appCycle :: Lens' (m a) (AppLifeCycle a)
  appCycle = appState . go where go f s = (\v -> s{_appCycle = v}) <$> f (_appCycle s)

  appInputListener :: Lens' (m a) (InputListener a)
  appInputListener = appState . go where go f s = (\v -> s{_appInputListener = v}) <$> f (_appInputListener s)

  appInput :: Lens' (m a) InputState
  appInput = appState . go where go f s = (\v -> s{_appInput = v}) <$> f (_appInput s)

  appWindow :: Lens' (m a) Window
  appWindow = appState . go where go f s = (\v -> s{_appWindow = v}) <$> f (_appWindow s)

  appStatus :: Lens' (m a) AppStatus
  appStatus = appState . go where go f s = (\v -> s{_appStatus = v}) <$> f (_appStatus s)

  appDeltaTime :: Lens' (m a) Double
  appDeltaTime = appState . go where go f s = (\v -> s{_appDeltaTime = v}) <$> f (_appDeltaTime s)

instance HasAppState AppState a where
  appState = id

-------------------------------------------------------------------------------
-- App Life Cycle -------------------------------------------------------------

data AppLifeCycle a =
  AppLifeCycle
  { _appCreate :: App a ()
  , _appResize :: V2 Int -- | Window size
               -> App a ()
  , _appUpdate :: App a ()
  , _appPause :: App a ()
  , _appResume :: App a ()
  , _appDispose :: App a ()
  }

defAppLifeCycle :: AppLifeCycle a
defAppLifeCycle =
  AppLifeCycle
    { _appCreate = return ()
    , _appResize = \ _ -> return ()
    , _appUpdate = return ()
    , _appPause = return ()
    , _appResume = return ()
    , _appDispose = return ()
    }

class HasAppLifeCycle m a where
  appLifeCycle :: Lens' (m a) (AppLifeCycle a)

  appCreate :: Lens' (m a) (App a ())
  appCreate = appLifeCycle . go
    where go f cy = (\appCreate' -> cy{_appCreate = appCreate'}) <$> f (_appCreate cy)

  appResize :: Lens' (m a) (V2 Int -> App a ())
  appResize = appLifeCycle . go
    where go f cy = (\appResize' -> cy{_appResize = appResize'}) <$> f (_appResize cy)

  appUpdate :: Lens' (m a) (App a ())
  appUpdate = appLifeCycle . go
    where go f cy = (\appUpdate' -> cy{_appUpdate = appUpdate'}) <$> f (_appUpdate cy)

  appPause :: Lens' (m a) (App a ())
  appPause = appLifeCycle . go
    where go f cy = (\appPause' -> cy{_appPause = appPause'}) <$> f (_appPause cy)

  appResume :: Lens' (m a) (App a ())
  appResume = appLifeCycle . go
    where go f cy = (\appResume' -> cy{_appResume = appResume'}) <$> f (_appResume cy)

  appDispose :: Lens' (m a) (App a ())
  appDispose = appLifeCycle . go
    where go f cy = (\appDispose' -> cy{_appDispose = appDispose'}) <$> f (_appDispose cy)

instance HasAppLifeCycle AppLifeCycle a where
  appLifeCycle = id

-------------------------------------------------------------------------------
-- Input Listener -------------------------------------------------------------

data InputListener a =
  InputListener
  { _keyReleased :: Key -> App a ()
  , _keyPressed :: Key -> App a ()
  , _keyHeld :: Key -> Double -> App a ()
  , _keyTyped :: Char -> App a ()

  , _mousePositioned :: V2 Double  -- ^ Current position of the mouse cursor
                     -> App a ()

  , _mouseMoved :: V2 Double  -- ^ Velocity of the mouse cursor
                -> App a ()
    
  , _mouseReleased :: MouseButton -- ^ Mouse button released
                   -> V2 Double   -- ^ Position of mouse cursor when released
                   -> App a ()

  , _mouseClicked :: MouseButton
                  -> V2 Double   -- ^ Position of mouse button when clicked
                  -> App a ()

  , _mouseClickHeld :: MouseButton
                    -> Double    -- ^ Time the mouse button has been held
                    -> V2 Double -- ^ Position of the mouse
                    -> App a ()

  , _mouseClickDragged :: MouseButton
                       -> Double    -- ^ Time the mouse button has been held
                       -> V2 Double -- ^ Velocity of the mouse
                       -> App a ()

  , _scrolled :: V2 Double -- ^ Amount scrolled
              -> App a ()
  }


defInputListener :: InputListener a
defInputListener =
  InputListener
    { _keyReleased = \ _ -> return ()
    , _keyPressed = \ _ -> return ()
    , _keyHeld = \ _ _ -> return ()
    , _keyTyped = \ _ -> return ()
    , _mousePositioned = \ _ -> return ()
    , _mouseMoved = \ _ -> return ()
    , _mouseReleased = \ _ _ -> return ()
    , _mouseClicked = \ _ _ -> return ()
    , _mouseClickHeld = \ _ _ _ -> return ()
    , _mouseClickDragged = \ _ _ _ -> return ()
    , _scrolled = \ _ -> return ()
    }


class HasInputListener m a where
    inputListener :: Lens' (m a) (InputListener a)

    keyReleased :: Lens' (m a) (Key -> App a ())
    keyReleased = inputListener . go
      where go f l = (\v -> l{_keyReleased = v}) <$> f (_keyReleased l)

    keyPressed :: Lens' (m a) (Key -> App a ())
    keyPressed = inputListener . go
      where go f l = (\v -> l{_keyPressed = v}) <$> f (_keyPressed l)

    keyHeld :: Lens' (m a) (Key -> Double -> App a ())
    keyHeld = inputListener . go
      where go f l = (\v -> l{_keyHeld = v}) <$> f (_keyHeld l)

    keyTyped :: Lens' (m a) (Char -> App a ())
    keyTyped = inputListener . go
      where go f l = (\v -> l{_keyTyped = v}) <$> f (_keyTyped l)

    mousePositioned :: Lens' (m a) (V2 Double -> App a ())
    mousePositioned = inputListener . go
      where go f l = (\v -> l{_mousePositioned = v}) <$> f (_mousePositioned l)

    mouseMoved :: Lens' (m a) (V2 Double -> App a ())
    mouseMoved = inputListener . go
      where go f l = (\v -> l{_mouseMoved = v}) <$> f (_mouseMoved l)

    mouseReleased :: Lens' (m a) (MouseButton -> V2 Double -> App a ())
    mouseReleased = inputListener . go
      where go f l = (\v -> l{_mouseReleased = v}) <$> f (_mouseReleased l)

    mouseClicked :: Lens' (m a) (MouseButton -> V2 Double -> App a ())
    mouseClicked = inputListener . go
      where go f l = (\v -> l{_mouseClicked = v}) <$> f (_mouseClicked l)

    mouseClickHeld :: Lens' (m a) (MouseButton -> Double -> V2 Double -> App a ())
    mouseClickHeld = inputListener . go
      where go f l = (\v -> l{_mouseClickHeld = v}) <$> f (_mouseClickHeld l)

    mouseClickDragged :: Lens' (m a) (MouseButton -> Double -> V2 Double -> App a ())
    mouseClickDragged = inputListener . go
      where go f l = (\v -> l{_mouseClickDragged = v}) <$> f (_mouseClickDragged l)

    scrolled :: Lens' (m a) (V2 Double -> App a ())
    scrolled = inputListener . go
      where go f l = (\v -> l{_scrolled = v}) <$> f (_scrolled l)


instance HasInputListener InputListener a where
    inputListener = id