module Basic where

import Control.Monad.Eff.Console (log)
import Graphics.WebGLAll (Attribute, Shaders(Shaders))
import Graphics.WebGLAll as Gl


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

shaders ::
  Shaders
    { aVertexPosition :: Attribute Gl.Vec3
    }
shaders = Shaders fragmentShader vertexShader

main = log "basic example"
