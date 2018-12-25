module Gfx
  ( App(..)
  , Config(..)
  , EffModel(..)
  , Renderer(..)
  , def
  , class Default
  , run
  , noEffects
  , onlyEffects
  )
where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Data.Typelevel.Undefined (undefined)
import Graphics.WebGLAll (Shaders(Shaders), WebGLProg, WebGl)
import Signal (Signal)

-- |
newtype EffModel st evt eff =
  EffModel { state :: st, effects :: Aff eff (Maybe evt) }

-- | Defines a structured WebGl program
newtype App st evt pic bind eff =
  App
    { init :: st
    , update :: evt -> st -> EffModel st evt eff
    , view :: st -> pic
    , signal :: Signal evt
    , renderer :: Renderer pic bind eff
    }

-- |
newtype Renderer pic bind eff =
  Renderer
    { render ::
        { webGLProgram :: WebGLProg | bind }
        -> pic
        -> Eff (webgl :: WebGl | eff) Unit
    , shaders :: Shaders { | bind }
    }

-- |
newtype Config st evt pic bind eff =
  Config
    { canvasId :: String
    , app :: App st evt pic bind eff
    }

-- | Run the app.
run :: forall st evt pic eff bind.
  Config st evt pic bind eff
  -> Eff ( webgl :: WebGl | eff ) Unit
run = undefined

-- | Create an `EffModel` with no effects from a given state.
noEffects :: forall st evt eff. st -> EffModel st evt eff
noEffects = undefined

-- | Create an `EffModel` with only effects from a given state.
onlyEffects :: forall st evt eff. st -> EffModel st evt eff
onlyEffects = undefined

class Default a where
  def :: a

instance defaultApp :: (Default st, Default evt, Default pic) => Default (App st evt pic eff bind) where
  def = App
    { init : def
    , update : def
    , view : def
    , signal : def
    , renderer : def
    }

instance defaultRenderer :: Default (Renderer pic bind eff) where
  def = Renderer
    { render : def
    , shaders : def
    }

instance defaultConfig :: Default (Config Unit Unit Unit bind eff) where
  def = Config
    { canvasId : def
    , app : def
    }

instance defaultString :: Default String where
  def = ""

instance defaultUnit :: Default Unit where
  def = unit

instance defaultFn :: (Default b) => Default (a -> b) where
  def = def

instance defaultEffModel :: (Default st) => Default (EffModel st evt eff) where
  def = EffModel { state : def, effects : def }

instance defaultAff :: (Default a) => Default (Aff eff a) where
  def = pure def

instance defaultEff :: (Default a) => Default (Eff eff a) where
  def = pure def

instance defaultMaybe :: Default (Maybe a) where
  def = Nothing

instance defaultSignal :: Default a => Default (Signal a) where
  def = pure def

instance defaultShaders :: Default (Shaders { | bind }) where
  -- @TODO: add default shader strings
  def = Shaders "" ""

type DefaultApp = forall eff bind. App Unit Unit Unit bind eff
