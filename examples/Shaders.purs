module Shaders where

import Graphics.WebGLAll (Attribute, Mat4, Shaders(Shaders), Uniform)
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
