{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, RankNTypes #-}
module Gx.Internal.Input
  ( module Gx.Internal.Input
  , module Gx.Internal.Data.Input
  )
where

import Control.Lens
import Control.Monad.Trans
import Data.IORef
import Linear

import qualified Data.Map.Lazy as Map


import Gx.Data.App
import Gx.Data.Input
import Gx.Data.Input.Listener
import Gx.Internal.Data.Input

import qualified Gx.Internal.Backend as B


-- Input caching --------------------------------------------------------------
-- Stores input events in a map for monitoring.
-- Removes inputs on release events.
cacheInput :: Ord k => InputEventMap k -> k -> B.InputState -> (Maybe InputEvent, InputEventMap k)
cacheInput evmap key event =
  let event' = B.fromBackend event
  in
    case event of
        B.Down ->
          case Map.lookup key evmap of
              Nothing ->
                (Just event', Map.insert key event' evmap)
              
              Just _ ->
                (Nothing, evmap)
        
        B.Up -> 
          (Just event', Map.delete key evmap)

cleanInputCache :: App ()
cleanInputCache = do
  appInput.inputKeyTable .= Map.empty
  appInput.inputButtonTable .= Map.empty

-- Input update --------------------------------------------------------------
-- | Updates the input in the tables
updateInput :: B.Backend a => IORef a -> App ()
updateInput backendRef = do
  t <- liftIO $ B.elapsedTime backendRef
  appInput.inputKeyTable %= Map.map (tickInputEvent t)
  appInput.inputButtonTable %= Map.map (tickInputEvent t)

  keys    <- use $ appInput . inputKeyTable
  buttons <- use $ appInput . inputButtonTable
  pos1    <- use $ appInput . inputMousePos1
  pos2    <- use $ appInput . inputMousePos2

  w <- use appWorld

  mapM_ (notifyEntityKeyListener keys) w
    --l <- use $ entityState . entityInputListener
    --es <- use entityState
    --evalStateT es $ Map.traverseWithKey (notifyKeyListener l) keys
    
    --l <- use entityInputListener -- has to re-grab data, which might have changed since last time
    --e <- use entityData
    --evalStateT e $ Map.traverseWithKey (notifyButtonListener l pos1 pos2) buttons

  appInput.inputMousePos1 .= pos2


notifyEntityKeyListener :: KeyMap -> EntityState' -> App ()
notifyEntityKeyListener keys (EntityState' e) =
  flip evalEntity e $ do
    l <- use entityInputListener
    mapM_ (notifyKeyListener l) (Map.toList keys)


notifyKeyListener :: InputListener e -> (Key, InputEvent) -> Entity e ()
notifyKeyListener l (key, ev) =
  case ev of
    Down ->
      _keyPressed l key
    Up ->
      _keyReleased l key
    Held _ dt ->
      _keyHeld l key dt


notifyButtonListener :: InputListener e -- ^ Input listener
                      -> V2 Double     -- ^ Last position of the mouse
                      -> V2 Double     -- ^ Newest position of the mouse
                      -> MouseButton   -- ^ Mouse button
                      -> InputEvent    -- ^ Input Event for mouse button
                      -> Entity e ()
notifyButtonListener l p1 p2 button ev =
  let dp = p2 - p1 in
  case ev of
    Down ->
      _mouseClicked l button p2
    Up ->
      _mouseReleased l button p2
    Held _ dt -> do
      _mouseClickHeld l button dt p2
      _mouseClickDragged l button dt dp

-- | Update the input held times.
-- | Upgrades Down to held, increments time on held, and does nothing when up.
tickInputEvent :: Double
                 -> InputEvent
                 -> InputEvent
tickInputEvent t ev =
  case ev of
    Down      -> Held t 0
    Held t1 _ -> Held t1 (t-t1)
    Up        -> ev


-- Input callbacks to communicate with backend --------------------------------
updateKeyboardInput :: B.Backend a => IORef a -> Key -> B.InputState -> App ()
updateKeyboardInput backendRef key state =
  do
    (maybeEvent', keys') <- uses (appInput.inputKeyTable)
                                $ \keys -> cacheInput keys key state
    appInput.inputKeyTable .= keys'
    
    case maybeEvent' of
        Nothing -> return ()
        Just state' -> return () --notifyKeyListeners listeners (key, state')


updateMouseMoveInput :: B.Backend a =>
                        IORef a ->
                        V2 Double ->
                        App ()
updateMouseMoveInput _ p2 = do
  p1 <- use (appInput.inputMousePos2) 
  let dp = p2-p1

  appInput.inputMousePos1 .= p1
  appInput.inputMousePos2 .= p2

  --mapM_ (\(MkInputListener a) -> mousePositioned a p2) listeners
  --mapM_ (\(MkInputListener a) -> mouseMoved a dp) listeners
  return ()

updateMouseClickInput ::  B.Backend a =>
                          IORef a ->
                          MouseButton ->
                          B.InputState -> 
                          V2 Double ->
                          App ()
updateMouseClickInput backendRef button ev p2 = do
  p1 <- use $ appInput.inputMousePos2
  appInput.inputMousePos1 .= p1
  appInput.inputMousePos2 .= p2

  (mayEv', buttons') <- uses (appInput.inputButtonTable)
                                  $ \buttons -> cacheInput buttons button ev
  
  appInput.inputButtonTable .= buttons'
  case mayEv' of
      Nothing -> return ()
      Just ev' -> return () --notifyButtonListeners listeners p1 p2 (button, state')


updateScrolledInput :: B.Backend a => IORef a
                        -> V2 Double -> App ()
updateScrolledInput _ scrollVel = do
  -- TODO: make velocity?
  --listeners <- appRef ^@ appInput.inputListeners
  --mapM_ (\(MkInputListener a) -> scrolled a scrollVel) listeners
  return ()
