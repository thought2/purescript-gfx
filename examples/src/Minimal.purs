module Minimal where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Gfx (App(App), DefaultApp, def, run)

app :: DefaultApp
app = App
  { init : def
  , update : def
  , view : def
  , signal: def
  , renderer : def
  }

main :: forall eff. Eff ( webgl :: WebGl | eff ) Unit
main = run { canvasId : "glcanvas", app }
