module Shaders where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.WebGL (WebGl)
import Gfx (Config, defaultConfig, run)
import Graphics.WebGLAll (Attribute, Shaders(Shaders))
import Graphics.WebGLAll as Gl


type Bindings = ( aVertexPosition :: Attribute Gl.Vec3 )

vertexShader :: String
vertexShader =
  """
    attribute vec3 aVertexPosition;

    void main(void) {
      gl_PointSize = 10.0;
      gl_Position = vec4(aVertexPosition, 1.0);
    }
  """

fragmentShader :: String
fragmentShader =
  """
    precision mediump float;

    void main(void) {
      gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
  """

shaders :: Shaders (Record Bindings)
shaders = Shaders fragmentShader vertexShader


config :: forall bind eff. Config Unit Unit Unit bind ( console :: CONSOLE | eff )
config = defaultConfig
  { canvasId = "glcanvas" }

main :: forall eff. Eff ( webgl :: WebGl, console :: CONSOLE | eff ) Unit
main = run config
