module BackgroundColor where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Gfx (Config, RenderFn, defaultConfig, defaultRenderer, run)
import Graphics.WebGLAll (Mask(COLOR_BUFFER_BIT), WebGl, clear, clearColor)


render :: forall bind eff. RenderFn Unit bind eff
render _ _ = do
  clearColor 0.2 0.4 0.7 1.0
  clear [ COLOR_BUFFER_BIT ]

config :: forall bind eff. Config Unit Unit Unit bind ( console :: CONSOLE | eff )
config = defaultConfig
  { canvasId = "glcanvas"
  , renderer = defaultRenderer
      { render = render
      }
  }

main :: forall eff. Eff ( webgl :: WebGl, console :: CONSOLE | eff ) Unit
main = run config
