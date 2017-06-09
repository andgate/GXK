{-# LANGUAGE RankNTypes
           , TemplateHaskell
           , GeneralizedNewtypeDeriving
           , ExistentialQuantification
           , DeriveFunctor
  #-}
module Gx.Data.App where

import Data.Map.Lazy (Map)
import Control.Lens
import Control.Monad.State
import Linear

import qualified Data.Map.Lazy as Map

import {-# SOURCE #-} Gx.Data.Input.Listener
import Gx.Data.Window

import Gx.Internal.Data.Input (mkInputState, InputState)


newtype App a = App { unApp :: StateT AppState IO a }
    deriving (Functor, Applicative, Monad, MonadState AppState, MonadIO)

newtype Entity e a = Entity { unEntity :: StateT (EntityState e) App a }
    deriving (Functor, Applicative, Monad, MonadState (EntityState e), MonadIO)

runEntity :: Entity e a -> EntityState e -> App (a, EntityState e)
runEntity e = runStateT (unEntity e)

evalEntity :: Entity e a -> EntityState e -> App a
evalEntity e = evalStateT (unEntity e)

execEntity :: Entity e a -> EntityState e -> App (EntityState e)
execEntity e = execStateT (unEntity e)

data AppListener =
  AppListener
  { _appCreate :: App ()
  , _appResize :: V2 Int -- | Window size
              -> App ()
  , _appUpdate :: App ()
  , _appDraw :: App ()
  , _appPostUpdate :: App ()
  , _appPause :: App ()
  , _appResume :: App ()
  , _appDispose :: App ()
  }

mkAppListener :: AppListener
mkAppListener =
  AppListener
    { _appCreate = return ()
    , _appResize = \ _ -> return ()
    , _appUpdate = return ()
    , _appDraw = return ()
    , _appPostUpdate = return ()
    , _appPause = return ()
    , _appResume = return ()
    , _appDispose = return ()
    }


data AppState = AppState
  { _appWorld     :: Map String EntityState'
  , _appListener  :: AppListener
  , _appInput     :: InputState
  , _appWindow    :: Window
  , _appStatus    :: AppStatus
  , _appDeltaTime :: Double
  }

data AppStatus = AppPlay | AppQuit


mkAppState :: AppState
mkAppState =
  AppState
    { _appWorld = Map.empty
    , _appListener = mkAppListener
    , _appInput = mkInputState
    , _appWindow = windowDefault
    , _appStatus = AppPlay
    , _appDeltaTime = 0
    }

data EntityState e =
  EntityState
  { _entityName :: String
  , _entityData :: e
  , _entityInputListener :: InputListener e
  }

instance Ord (EntityState e) where
    compare a b = _entityName a `compare` _entityName b

instance Eq (EntityState e) where
    (==) a b = _entityName a == _entityName b


data EntityState' = forall e. EntityState' (EntityState e)

makeLenses ''AppListener
makeLenses ''AppState
makeLenses ''EntityState