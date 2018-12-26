module Points where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Gfx (Config, RenderFn, defaultConfig, defaultRenderer, defaultShaders, run)
import Graphics.WebGLAll (Attribute, Mode(POINTS), WebGl, drawArr, makeBufferFloat)
import Graphics.WebGLAll as Gl


type Bindings = ( aVertexPosition :: Attribute Gl.Vec3 )

render :: forall eff. RenderFn Unit Bindings eff
render { aVertexPosition } _ = do
  buf <- makeBufferFloat
    [ -1.0, -1.0, 0.0
    , -0.5, -0.5, 0.0
    ,  0.0,  0.0, 0.0
    ,  0.5,  0.5, 0.0
    ,  1.0,  1.0, 0.0
    ]
  drawArr POINTS buf aVertexPosition

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
    void main(void) {
      gl_FragColor = vec4(0.5, 0.5, 0.5, 1.0);
    }
  """

config :: forall eff.
  Config Unit Unit Unit Bindings ( console :: CONSOLE | eff )
config = defaultConfig
  { canvasId = "glcanvas"
  , renderer = defaultRenderer
      { shaders = defaultShaders
          { vertex = vertexShader
          , fragment = fragmentShader
          }
      , render = render
      }
  }

main :: forall eff. Eff ( webgl :: WebGl, console :: CONSOLE | eff ) Unit
main = run config
