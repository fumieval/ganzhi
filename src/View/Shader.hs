{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module View.Shader where

import Graphics.Holz
import Control.Monad
import Linear
import Graphics.Holz.Shader
import Graphics.Holz.Shader.Simple as S
import Data.Function (fix)
import qualified Text.RawString.QQ as QQ

vertexShaderSource :: VertexShaderSource ModelProj Vertex Fragment
vertexShaderSource = [QQ.r|
void main(void) {
  fPos = mpModel * vec4(vPosition, 1.0);
  gl_Position = mpProjection * fPos;
  fTexUV = vUV;
  fColor = vRGBA;
}
|]

fragmentShaderSource :: FragmentShaderSource ModelProj Fragment
fragmentShaderSource = [QQ.r|
uniform sampler2D tex;
out vec4 fragColor;
void main(void){
    fragColor = fColor * texture(tex, fTexUV);
}
|]
