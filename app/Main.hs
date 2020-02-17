{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Graphics.Holz
import Control.Monad
import Linear
import Graphics.Holz.Shader
import Graphics.Holz.Font
import qualified Graphics.Holz.Shader.Simple as S
import Data.Function (fix)
import qualified View as V
import qualified View.Shader as V
import qualified Graphics.Holz.Typeset as Text
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Model
import Types
import Data.List (zipWith, repeat)

data Env = Env
  { eWindow :: !Window
  , eShader :: !(Shader S.ModelProj S.Vertex)
  , eView :: V.Env
  }

instance V.HasEnv Env where getEnv = eView
instance HasWindow Env where getWindow = eWindow
instance HasShader Env where
  type ShaderUniform Env = S.ModelProj
  type ShaderVertex Env = S.Vertex
  getShader = eShader

testCards :: [Entity Card]
testCards = zipWith (\i -> Entity (EID i)) [0..6] (repeat testCard) where
  testCard = Card "Test" 1 ENone

main :: IO ()
main = withHolz $ do
  win <- openWindow Resizable (Box (V2 0 0) (V2 1024 768))
  sh <- makeShader V.vertexShaderSource V.fragmentShaderSource
  void $ V.acquire $ \envV -> flip runReaderT (Env win sh envV) $ do

    retract $ runHolz $ fix `flip` 0 `flip` V.hand $ \self t dh -> do
      box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
      setViewport box
      S.setProjection $ ortho x0 x1 y1 y0 (-1) 1

      lift $ V.drawBackground box

      dh' <- stepComponent testCards dh

      r <- ask
      liftIO $ uncurry withVertex (S.rectangle (pure 1) (V2 0 0) (V2 640 480))
        $ \v -> runReaderT (S.drawVertex identity (Text.atlas $ V.cardFont envV) v) r


      liftIO $ threadDelay 16000
      delay $ self (t + 1) dh'
