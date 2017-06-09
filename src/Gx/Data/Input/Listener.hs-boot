{-# LANGUAGE RoleAnnotations #-}
module Gx.Data.Input.Listener where

type role InputListener nominal
data InputListener e

mkInputListener :: InputListener e