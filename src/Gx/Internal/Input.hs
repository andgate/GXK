{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables, RankNTypes #-}
module Gx.Internal.Input where

import Control.Lens
import Control.Monad (join, forM_)
import Linear

import qualified Data.Map.Lazy as Map


import Gx.Data.App
import Gx.Data.Input

import qualified Gx.Internal.Backend.Types as B


-- Input caching --------------------------------------------------------------
-- Stores input events in a map for monitoring.
-- Removes inputs on release events.
cacheInput :: Ord k => InputEventMap k -> k -> B.InputEvent -> (Maybe InputEvent, InputEventMap k)
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

cleanInputCache :: App w ()
cleanInputCache = do
  appInput.inputKeyTable .= Map.empty
  appInput.inputButtonTable .= Map.empty

-- Input update --------------------------------------------------------------
-- | Updates the input in the tables
updateInput :: App w ()
updateInput = do
  dt <- use appDeltaTime
  inputKeyTable %= Map.map (tickInputEvent dt)
  inputButtonTable %= Map.map (tickInputEvent dt)

  keys    <- use $ appInput . inputKeyTable
  buttons <- use $ appInput . inputButtonTable
  pos1    <- use $ appInput . inputMousePos1
  pos2    <- use $ appInput . inputMousePos2
  
  Map.traverseWithKey keyNotify keys
  Map.traverseWithKey (buttonNotify pos1 pos2) buttons

  appInput.inputMousePos1 .= pos2


keyNotify :: Key -> InputEvent -> App w ()
keyNotify key ev =
  case ev of
    Down -> join $
      use (appInputListener . keyPressed) <*> pure key

    Up -> join $
      use (appInputListener . keyReleased) <*> pure key
    
    Held _ dt -> join $
      use (appInputListener . keyHeld) <*> pure key <*> pure dt


buttonNotify :: V2 Double     -- ^ Last position of the mouse
                      -> V2 Double     -- ^ Newest position of the mouse
                      -> MouseButton   -- ^ Mouse button
                      -> InputEvent    -- ^ Input Event for mouse button
                      -> App w ()
buttonNotify p1 p2 button ev =
  let dp = p2 - p1 in
  case ev of
    Down -> join $
      use (appInputListener . mouseClicked) <*> pure button <*> pure p2
    
    Up -> join $
      use (appInputListener . mouseReleased) <*> pure button <*> pure p2
    
    Held _ dt -> join $ do
      use (appInputListener . mouseClickHeld) <*> pure button <*> pure dt <*> pure p2
      use (appInputListener . mouseClickDragged) <*> pure button <*> pure dt <*> pure dp


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
updateKeyboardInput :: Key -> B.InputEvent -> App w ()
updateKeyboardInput key ev =
  do
    (may_ev', keys') <- uses (appInput.inputKeyTable)
                                $ \keys -> cacheInput keys key ev
    appInput.inputKeyTable .= keys'
    
    forM_ may_ev' (keyNotify key)


updateMouseMoveInput :: V2 Double ->
                        App w ()
updateMouseMoveInput p2 = do
  p1 <- use (appInput.inputMousePos2) 
  let dp = p2-p1

  appInput.inputMousePos1 .= p1
  appInput.inputMousePos2 .= p2

  join $ do
    use (appInputListener.mousePositioned) <*> pure p2
    use (appInputListener.mouseMoved) <*> pure dp


updateMouseClickInput ::  MouseButton ->
                          B.InputEvent -> 
                          V2 Double ->
                          App w ()
updateMouseClickInput button ev p2 = do
  p1 <- use $ appInput.inputMousePos2
  appInput.inputMousePos1 .= p1
  appInput.inputMousePos2 .= p2

  (may_ev', buttons') <- uses (appInput.inputButtonTable)
                                  $ \buttons -> cacheInput buttons button ev
  
  appInput.inputButtonTable .= buttons'
  forM_ may_ev' (buttonNotify p1 p2 button)


updateScrolledInput :: V2 Double -> App w ()
updateScrolledInput scrollVel =
  -- TODO: make velocity?
  join $ use (appInputListener.scrolled) <*> pure scrollVel
