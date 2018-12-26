module Gfx
  ( App(..)
  , Config(..)
  , Renderer(..)
  , RenderFn
  , def
  , class Default
  , run
  , Err
  , Shaders
  , errMessage
  , defaultApp
  , defaultConfig'
  , defaultConfig
  , defaultRenderer
  , defaultShaders
  )
where

import Prelude

import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (throw, message, try)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Graphics.WebGLAll (WebGLProg, WebGl, WebGLContext)
import Graphics.WebGLAll as WebGl
import Signal (Signal, foldp, runSignal, (~>))


-- | Defines a structured WebGl program.
type App st evt pic =
  { init :: st
  , update :: evt -> st -> st
  , view :: st -> pic
  , signal :: Signal evt
  }

-- | A combination of a render function and some shaders.
type Renderer pic bind eff =
  { render :: RenderFn pic bind eff
  , shaders :: Shaders { | bind }
  }

-- | Takes a custom `pic` type (emitted by `App`) and the shader's bindings.
type RenderFn pic bind eff =
  { webGLProgram :: WebGLProg | bind }
  -> pic
  -> Eff (webgl :: WebGl | eff) Unit

type Shaders bind =
  { vertex :: String
  , fragment :: String
  }

-- | WebGl related errors.
data Err = ErrRun String | ErrShaders String | ErrAsync

-- | Convert `Err` to human readable error message.
errMessage :: Err -> String
errMessage err =
  case err of
    ErrRun msg -> format "ErrRun" (Just msg)
    ErrShaders msg -> format "ErrShader" (Just msg)
    ErrAsync -> format "ErrAsync" Nothing
  where
    format name msg = "ERROR: " <> name <> "\n" <> maybe "" id msg

-- | Config used for `run` (Error handling: `ExceptT`).
type Config' st evt pic bind eff =
  { canvasId :: String
  , app :: App st evt pic
  , renderer :: Renderer pic bind eff
  }

-- | config used for `run` (Error handling: callback).
type Config st evt pic bind eff =
  { canvasId :: String
  , app :: App st evt pic
  , renderer :: Renderer pic bind eff
  , handleError :: Err -> Eff ( webgl :: WebGl | eff) Unit
  }

-- | Run the app as `ExceptT`.
run' :: forall st evt pic bind eff.
  Config st evt pic bind eff -> ExceptT Err (Aff ( webgl ∷ WebGl | eff )) Unit
run'
  { canvasId
  , app : { init, update, view, signal }
  , renderer : { render, shaders : { vertex, fragment } }
  } = do
    context <- runWebGL canvasId
    bindings <- withShaders ((WebGl.Shaders fragment vertex) :: WebGl.Shaders { | bind })
    liftEff $ do
      runSignal (sigState ~> (\state -> render bindings $ view state))
      render bindings (view init)
    where
      sigState = foldp update init signal

-- | Run the app.
run :: forall st evt pic eff bind.
  Config st evt pic bind eff
  -> Eff ( webgl :: WebGl | eff ) Unit
run config = map (const unit) $ launchAff $ do
  result <- runExceptT $ run' config
  case result of
    Left err -> liftEff $ config.handleError err
    Right _ -> pure unit

-- | Wrapper around native `runWebGl`.
runWebGL :: forall eff.
  String
  -> ExceptT Err (Aff ( webgl ∷ WebGl | eff )) WebGLContext
runWebGL str =
  WebGl.runWebGL str throw pure
   #  try
  <#> lmap (message >>> ErrRun)
   #  liftEff
   #  wrap

-- | Wrapper around native `withShaders`.
withShaders :: forall eff a.
  WebGl.Shaders { | a }
  -> ExceptT Err (Aff ( webgl :: WebGl | eff )) { webGLProgram :: WebGLProg | a }
withShaders shaders =
  WebGl.withShaders shaders throw pure
   #  try
  <#> lmap (message >>> ErrShaders)
   #  liftEff
   #  wrap

-- Defaults

class Default a where
  def :: a

instance defaultUnit :: Default Unit where
  def = unit

instance defaultFn :: (Default b) => Default (a -> b) where
  def _ = def

instance defaultAff :: (Default a) => Default (Aff eff a) where
  def = pure def

instance defaultEff :: (Default a) => Default (Eff eff a) where
  def = pure def

instance defaultMaybe :: Default (Maybe a) where
  def = Nothing

instance defaultSignal :: Default a => Default (Signal a) where
  def = pure def

instance defaultString :: Default String where
  def = ""

defaultApp :: forall st evt pic.
  Default st => Default evt => Default pic => App st evt pic
defaultApp =
  { init : def
  , update : def
  , view : def
  , signal : def
  }

defaultRenderer :: forall pic bind eff. Renderer pic bind eff
defaultRenderer =
  { render : def
  , shaders : defaultShaders
  }

defaultConfig' :: forall st evt pic bind eff.
  Default st => Default evt => Default pic => Config' st evt pic bind eff
defaultConfig' =
  { canvasId : def
  , app : defaultApp
  , renderer : defaultRenderer
  }

defaultConfig :: forall st evt pic bind eff.
  Default st => Default evt => Default pic =>
  Config st evt pic bind ( console :: CONSOLE | eff)
defaultConfig =
  { canvasId : def
  , app : defaultApp
  , renderer : defaultRenderer
  , handleError : errMessage >>> log
  }

defaultShaders :: forall bind. Shaders bind
defaultShaders =
  { vertex : defaultShader
  , fragment : defaultShader
  }
  where
    defaultShader = "void main(void) {}"
