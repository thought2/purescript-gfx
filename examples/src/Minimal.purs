module Minimal where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.WebGL (WebGl)
import Gfx (Config, defaultConfig, run)

config :: forall bind eff. Config Unit Unit Unit bind ( console :: CONSOLE | eff )
config = defaultConfig
  { canvasId = "glcanvas" }

main :: forall eff. Eff ( webgl :: WebGl, console :: CONSOLE | eff ) Unit
main = run config
