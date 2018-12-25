module Shaders where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.WebGL (WebGl)
import Gfx (DefaultApp, Renderer, def, run)
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

app :: DefaultApp
app =
  { init : def
  , update : def
  , view : def
  , signal: def
  }

renderer :: forall eff. Renderer Unit Bindings eff
renderer =
  { render : def
  , shaders
  }

main :: forall eff. Eff ( webgl :: WebGl | eff ) Unit
main = run { canvasId : "glcanvas", app, renderer }
