module Minimal where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Gfx (DefaultApp, DefaultRenderer, def, run)

app :: DefaultApp
app =
  { init : def
  , update : def
  , view : def
  , signal: def
  }

renderer :: DefaultRenderer
renderer =
  { render : def
  , shaders : def
  }

main :: forall eff. Eff ( webgl :: WebGl | eff ) Unit
main = run { canvasId : "glcanvas", app, renderer }
