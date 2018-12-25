module Minimal where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.WebGL (WebGl)
import Gfx (App, Renderer, def, run)

app :: App Unit Unit Unit
app =
  { init : def
  , update : def
  , view : def
  , signal: def
  }

renderer :: forall bind eff. Renderer Unit bind eff
renderer =
  { render : def
  , shaders : def
  }

main :: forall eff. Eff ( webgl :: WebGl, console :: CONSOLE | eff ) Unit
main = run { canvasId : "glcanvas", app, renderer } (\_ -> log "error")
