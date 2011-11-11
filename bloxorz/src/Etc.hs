module Etc where

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

import Paths_bloxorz (getDataFileName)


double2float :: Double -> Float
--double2float = fromRational . toRational
double2float = realToFrac


piPer2 :: GLfloat
piPer2 = pi / 2.0


vecNeg :: Vector3 GLfloat -> Vector3 GLfloat
vecNeg (Vector3 x y z) = Vector3 (-x) (-y) (-z)

cube :: IO ()
cube = renderPrimitive Quads $ do
    -- back
    n 0 0 (-1)
    t 0 1 >> v (-1) (-1) (-1)
    t 1 1 >> v   1  (-1) (-1)
    t 1 0 >> v   1    1  (-1)
    t 0 0 >> v (-1)   1  (-1)
    -- front
    n 0 0 1
    t 0 1 >> v   1  (-1)   1
    t 1 1 >> v (-1) (-1)   1
    t 1 0 >> v (-1)   1    1
    t 0 0 >> v   1    1    1
    -- left
    n (-1) 0 0
    t 0 1 >> v (-1) (-1)   1
    t 1 1 >> v (-1) (-1) (-1)
    t 1 0 >> v (-1)   1  (-1)
    t 0 0 >> v (-1)   1    1
    -- right
    n 1 0 0
    t 0 1 >> v   1  (-1) (-1)
    t 1 1 >> v   1  (-1)   1
    t 1 0 >> v   1    1    1
    t 0 0 >> v   1    1  (-1)
    -- top
    n 0 1 0
    t 0 1 >> v   1    1  (-1)
    t 1 1 >> v   1    1    1
    t 1 0 >> v (-1)   1    1
    t 0 0 >> v (-1)   1  (-1)
    -- bottom
    n 0 (-1) 0
    t 0 1 >> v   1  (-1)   1
    t 1 1 >> v   1  (-1) (-1)
    t 1 0 >> v (-1) (-1) (-1)
    t 0 0 >> v (-1) (-1)   1
    where v x y z = GL.vertex (GL.Vertex3 x y z :: GL.Vertex3 GLfloat)
          n x y z = GL.normal (GL.Normal3 x y z :: GL.Normal3 GLfloat)
          t u v   = GL.texCoord (GL.TexCoord2 u v :: GL.TexCoord2 GLfloat)


quad :: GLfloat -> IO ()
quad z = renderPrimitive Quads $ do
    t 0 1 >> v (-1)   1    z
    t 1 1 >> v   1    1    z
    t 1 0 >> v   1  (-1)   z
    t 0 0 >> v (-1) (-1)   z
    where v x y z = GL.vertex (GL.Vertex3 x y z :: GL.Vertex3 GLfloat)
          t u v   = GL.texCoord (GL.TexCoord2 u v :: GL.TexCoord2 GLfloat)


loadTexture :: String -> IO GL.TextureObject
loadTexture filename = do
  dataFileName <- getDataFileName filename
  [texName] <- GL.genObjectNames 1
  GL.textureBinding Texture2D $= Just texName
  GLFW.loadTexture2D dataFileName [GLFW.BuildMipMaps]
  GL.textureFilter Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear') -- trilinear filtering
  return texName
