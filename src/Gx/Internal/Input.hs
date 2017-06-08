{-# LANGUAGE ExistentialQuantification #-}
module Gx.Internal.Input
  ( module Gx.Internal.Input
  , module Gx.Internal.Data.Input
  )
where

import Gx.Data.App
import Gx.Data.Input
import Gx.Internal.Data.Input
import qualified Gx.Internal.Backend as B

import Control.Arrow
import Control.Lens
import Control.Monad
import Data.IORef
import Data.Hashable
import qualified Data.HashTable.IO as H
import Gx.Data.IORef.Lens
import Linear


-- Input caching --------------------------------------------------------------
-- Stores input in table for monitoring held keys.
-- Removes input on release. This keeps the tables small.
cacheInput :: (Hashable k, Eq k) => InputTable k -> k -> B.InputState -> IO (Maybe InputState)
cacheInput table key state = do
  let state' = B.fromBackend state
  case state of
    B.Down -> do
      maybeOldState <- H.lookup table key
      case maybeOldState of
        Nothing -> do
          H.insert table key state'
          return $ Just state'
        Just _ -> return Nothing
    B.Up -> do
      H.delete table key
      return $ Just state'

cleanInputCache :: AppRef w -> IO ()
cleanInputCache appRef = do
  keyTable <- H.new
  buttonTable <- H.new
  appRef & appInput.inputKeyTable @~ keyTable
  appRef & appInput.inputButtonTable @~ buttonTable

-- Input update --------------------------------------------------------------
-- | Updates the input in the tables
updateInput :: B.Backend a => AppRef w -> IORef a -> IO ()
updateInput appRef backendRef = do
  input <- appRef ^@ appInput
  let keyTable    = input^.inputKeyTable
      buttonTable = input^.inputButtonTable
      listeners   = input^.inputListeners
      pos1        = input^.inputMousePos1
      pos2        = input^.inputMousePos2

  t <- B.elapsedTime backendRef
  updateInputTable t keyTable
  updateInputTable t buttonTable

  H.mapM_ (notifyKeyListeners listeners) keyTable
  H.mapM_ (notifyButtonListeners listeners pos1 pos2) buttonTable

  appRef & appInput.inputMousePos1 @~ pos2

notifyKeyListeners :: [Registrable] -> (Key, InputState) -> IO ()
notifyKeyListeners listeners (key, state) =
  case state of
    Down ->
      mapM_ (\(MkInputListener a) -> keyPressed a key) listeners
    Up ->
      mapM_ (\(MkInputListener a) -> keyReleased a key) listeners
    Held _ dt ->
      mapM_ (\(MkInputListener a) -> keyHeld a key dt) listeners

notifyButtonListeners :: [Registrable] -- ^ List of input listeners
                      -> V2 Double     -- ^ Last position of the mouse
                      -> V2 Double     -- ^ Newest position of the mouse
                      -> (MouseButton, InputState) -- ^ Mouse button and it's associated input state
                      -> IO ()
notifyButtonListeners listeners p1 p2 (button, state) =
  let dp = p2 - p1 in
  case state of
    Down ->
      mapM_ (\(MkInputListener a) -> mouseClicked a button p2) listeners
    Up ->
      mapM_ (\(MkInputListener a) -> mouseReleased a button p2) listeners
    Held _ dt -> do
      mapM_ (\(MkInputListener a) -> mouseClickHeld a button dt p2) listeners
      mapM_ (\(MkInputListener a) -> mouseClickDragged a button dt dp) listeners

-- | Upgrades Down to held, increments time on held, and does nothing when up.
updateInputState :: Double
                 -> (k, InputState)
                 -> IO (k, InputState)
updateInputState t (k, state) =
  case state of
    Down      -> return (k, Held t 0)
    Held t1 _ -> return (k, Held t1 (t-t1))
    Up        -> return (k, state)

-- | Updates the states of an input table
updateInputTable :: (Hashable k, Eq k)
                 => Double -> InputTable k -> IO ()
updateInputTable t table =
  H.toList table >>= mapM_ (updateInputState t >=> uncurry (H.insert table))


-- Input callbacks to communicate with backend --------------------------------
updateKeyboardInput :: B.Backend a => AppRef w -> IORef a -> Key -> B.InputState -> IO ()
updateKeyboardInput appRef backendRef key state = do
  input <- appRef ^@ appInput
  let listeners = input^.inputListeners
  maybeState' <- cacheInput (input^.inputKeyTable) key state
  case maybeState' of
    Nothing -> return ()
    Just state' -> notifyKeyListeners listeners (key, state')

updateMouseMoveInput :: B.Backend a => AppRef w -> IORef a
                        -> V2 Double -> IO ()
updateMouseMoveInput appRef _ p2 = do
  input <- appRef ^@ appInput
  let listeners = input^.inputListeners

      -- Store the previous mouse position
      p1 = input^.inputMousePos2
      dp = p2-p1

  appRef & appInput.inputMousePos1 @~ p1
  appRef & appInput.inputMousePos2 @~ p2

  mapM_ (\(MkInputListener a) -> mousePositioned a p2) listeners
  mapM_ (\(MkInputListener a) -> mouseMoved a dp) listeners

updateMouseClickInput :: B.Backend a => AppRef w -> IORef a
                        -> MouseButton -> B.InputState
                        -> V2 Double -> IO ()
updateMouseClickInput appRef backendRef button state p2 = do
  input <- appRef ^@ appInput
  let listeners = input^.inputListeners
      p1 = input^.inputMousePos2

  appRef & appInput.inputMousePos1 @~ p1
  appRef & appInput.inputMousePos2 @~ p2

  maybeState' <- cacheInput (input^.inputButtonTable) button state
  case maybeState' of
    Nothing -> return ()
    Just state' -> notifyButtonListeners listeners p1 p2 (button, state')

updateScrolledInput :: B.Backend a => AppRef w -> IORef a
                        -> V2 Double -> IO ()
updateScrolledInput appRef _ scrollVel = do
  -- would be nice if this could be velocity
  listeners <- appRef ^@ appInput.inputListeners
  mapM_ (\(MkInputListener a) -> scrolled a scrollVel) listeners
