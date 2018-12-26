module Animation where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (range)
import Data.Int (toNumber)
import Gfx (Config, RenderFn, UpdateFn, ViewFn, defaultConfig, defaultRenderer, defaultShaders, run)
import Graphics.WebGLAll (Attribute, Mode(POINTS), WebGl, drawArr, makeBufferFloat)
import Graphics.WebGLAll as Gl
import Math (cos, pi, sin)
import Signal (Signal)
import Signal.Time (every)


type Bindings = ( aVertexPosition :: Attribute Gl.Vec3 )

data Point = Point Number Number

data Picture = PicPoints (Array Point)

type State = { tick :: Int }

data Event = Tick

view :: ViewFn State Picture
view { tick } =
  range 0 tick
  # map (toNumber >>> (_ * step) >>> (\t -> Point (cos t) (sin t)))
  # PicPoints
  where
    step = (pi + pi) / toNumber tick

render :: forall eff. RenderFn Picture Bindings eff
render { aVertexPosition } pic =
  case pic of
    PicPoints pts -> do
      buf <- makeBufferFloat (pts >>= (\(Point x y) -> [x, y, 0.0]))
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

init :: State
init = { tick : 0 }

update :: UpdateFn State Event
update event state = case event of
  Tick -> state { tick = (state.tick + 1) `mod` 100 }

signal :: Signal Event
signal = every 100.0 # map (const Tick)

config :: forall eff.
  Config State Event Picture Bindings ( console :: CONSOLE | eff )
config = defaultConfig
  { canvasId = "glcanvas"
  , app =
      { init
      , update
      , view
      , signal
      }
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
