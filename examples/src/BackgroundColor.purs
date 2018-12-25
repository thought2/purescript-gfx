module BackgroundColor where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Gfx (App, Renderer, RenderFn, def, run)
import Graphics.WebGLAll (Capacity(..), Mask(..), WebGl, clear, clearColor, enable)


app :: App Unit Unit Unit
app =
  { init : def
  , update : def
  , view : def
  , signal: def
  }

renderer :: forall bind eff. Renderer Unit bind eff
renderer =
  { render
  , shaders : def
  }

render :: forall bind eff. RenderFn Unit bind eff
render _ _ = do
  enable DEPTH_TEST
  clearColor 0.2 0.4 0.7 1.0
  clear [ COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT ]


main :: forall eff. Eff ( webgl :: WebGl, console :: CONSOLE | eff ) Unit
main = run { canvasId : "glcanvas", app, renderer } (\_ -> log "error")
