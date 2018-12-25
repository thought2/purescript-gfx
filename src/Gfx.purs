module Gfx
  ( App(..)
  , Config(..)
  , EffModel(..)
  , Renderer(..)
  , def
  , class Default
  , DefaultApp
  , DefaultRenderer
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
type App st evt pic eff =
  { init :: st
  , update :: evt -> st -> EffModel st evt eff
  , view :: st -> pic
  , signal :: Signal evt
  }

-- |
type Renderer pic bind eff =
  { render ::
      { webGLProgram :: WebGLProg | bind }
      -> pic
      -> Eff (webgl :: WebGl | eff) Unit
  , shaders :: Shaders { | bind }
  }

-- |
type Config st evt pic bind eff =
  { canvasId :: String
  , app :: App st evt pic eff
  , renderer :: Renderer pic bind eff
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


-- Defaults

class Default a where
  def :: a

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
  def = Shaders defShader defShader
    where
      defShader = "void main(void) {}"

type DefaultApp = forall eff bind. App Unit Unit Unit eff

type DefaultRenderer = forall eff bind. Renderer Unit bind eff
