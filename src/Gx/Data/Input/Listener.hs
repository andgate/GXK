{-# LANGUAGE TemplateHaskell #-}
module Gx.Data.Input.Listener where

import Control.Lens
import Linear

import Gx.Data.App
import Gx.Data.Input

data InputListener e =
  InputListener
  { _keyReleased :: Key -> Entity e ()
  , _keyPressed :: Key -> Entity e ()
  , _keyHeld :: Key -> Double -> Entity e ()
  , _keyTyped :: Char -> Entity e ()

  , _mousePositioned :: V2 Double  -- ^ Current position of the mouse cursor
                     -> Entity e ()

  , _mouseMoved :: V2 Double  -- ^ Velocity of the mouse cursor
                -> Entity e ()
    
  , _mouseReleased :: MouseButton -- ^ Mouse button released
                   -> V2 Double   -- ^ Position of mouse cursor when released
                   -> Entity e ()

  , _mouseClicked :: MouseButton
                  -> V2 Double   -- ^ Position of mouse button when clicked
                  -> Entity e ()

  , _mouseClickHeld :: MouseButton
                    -> Double    -- ^ Time the mouse button has been held
                    -> V2 Double -- ^ Position of the mouse
                    -> Entity e ()

  , _mouseClickDragged :: MouseButton
                       -> Double    -- ^ Time the mouse button has been held
                       -> V2 Double -- ^ Velocity of the mouse
                       -> Entity e ()

  , _scrolled :: V2 Double -- ^ Amount scrolled
              -> Entity e ()
  }


mkInputListener :: InputListener e
mkInputListener =
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

makeLenses ''InputListener