{-# LANGUAGE RecordWildCards, TypeFamilies, ExistentialQuantification #-}
module View where

import Graphics.Holz
import qualified Graphics.Holz.Typeset as Text
import qualified Graphics.Holz.Shader.Simple as S
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Control.Monad.Reader
import Codec.Picture.RGBA8
import Linear
import UnliftIO (MonadUnliftIO)
import Data.Semigroup
import Data.List (zip, minimum, map)
import Model
import Types

data Env = Env
  { gameBg :: Texture
  , cardBg :: Texture
  , cardFrame :: VertexBuffer S.Vertex
  , cardFont :: Text.Typeset
  , sampleText :: Text.Renderable S.Vertex
  }

instance Text.FromV2 S.Vertex where
  fromV2 (V2 x y) uv = S.Vertex (V3 x y 0) (V4 1 1 1 1) uv

class HasEnv a where
  getEnv :: a -> Env

type View = Component (Maybe (First EID))

acquire :: MonadUnliftIO m => (Env -> m r) -> m r
acquire = runContT $ do
  gameBg <- liftIO (readImageRGBA8 "resources/background.png") >>= registerTexture
  cardBg <- liftIO (readImageRGBA8 "resources/card-template.png") >>= registerTexture
  cardFrame <- ContT $ uncurry withVertex
    $ S.rectangle (V4 1 1 1 1) (V2 (-180) (-240)) (V2 180 240)
  font <- readFont "/System/Library/Fonts/LucidaGrande.ttc"
  cardFont <- Text.createTypeset font 64
  sampleText <- Text.string cardFont "Hello, haskellers"
  return Env{..}

drawBackground :: (S.HasSimpleShader r, MonadHolz r m, HasEnv r, MonadUnliftIO m) => Box V2 Float -> m ()
drawBackground (Box p q) = do
  Env{..} <- asks getEnv
  uncurry withVertex (S.rectangle 1 p q)
    $ \vb -> S.drawVertex identity gameBg vb

hand :: (S.HasSimpleShader r, MonadHolz r m, HasEnv r, HasWindow r) => View m [Entity Card]
hand = Component
  { cState = (mempty, 0 :: Float)
  , cUpdate = \cards (focus, elapsed) -> do
    Env{..} <- asks getEnv
    V2 cx cy <- getCursorPos

    let mats = do
          i <- [0 :: Int ..count - 1]
          let rot = pi * (fromIntegral i / fromIntegral count - 0.5) / 6
          return $! mkTransformation (axisAngle (V3 0 0 1) rot)
                (V3 origX (origY + ofs) 0)
                !*! S.translate (V3 0 (-ofs) 0)
    let invMats = map inv44 mats

    distances <- forM (zip cards invMats) $ \(i, inv) -> do
      let V4 tx ty _ _ = inv !* V4 cx cy 0 1
      return $! Min $ Arg (norm $ V2 tx ty) (eid i)

    let focus' = case minimum distances of
          Min (Arg dist i)
            | dist < 360 -> pure $ First i
            | otherwise -> mempty
    let elapsed'
          | focus' == focus = elapsed + 1
          | otherwise = 0

    forM_ (zip cards mats) $ \(Entity i _, mat) -> do
      let t = 1 - exp (-elapsed' / 5)
      let k = 1 + 0.3 * t
      let mat'
            | pure (First i) == focus = S.translate (t *^ V3 0 (-60) 1) !*! mat !*! scaled (V4 k k k 1)
            | otherwise = mat
      S.drawVertex mat' cardBg cardFrame
      Text.draw mat' sampleText

    return (focus', elapsed')
  , cExpose = fst
  }
  where
    count = 7
    ofs = 3600 -- rotaton radius
    origX = 1200
    origY = 800
