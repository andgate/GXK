{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gx.Internal.Backend.Types
  ( module Gx.Internal.Backend.Types
  , module Gx.Data.Window
  )
where


import Gx.Data.Window
import qualified Gx.Data.Input as I


data InputEvent = Up | Down

class ConvBackend a b where
  fromBackend :: a -> b

instance ConvBackend InputEvent I.InputEvent where
  fromBackend ev =
    case ev of
      Up -> I.Up
      Down -> I.Down
