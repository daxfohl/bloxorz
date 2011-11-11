module Main (
    main
) where

import Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GLU as GLU
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Control.Monad

-- Bloxorz stuff
import Maps
import Etc
import Cubezor


data GameState = GsStarting | GsGame | GsFalling | GsWinning deriving (Eq,Show)

data GameGlobal = GameGlobal { gamestate :: GameState
                             , fade :: GLfloat
                             , levelindex :: Int
                             } deriving Show

-- UberState
data State = State { level :: Maps.Level
                   , cubex :: Cube
                   , global :: GameGlobal
                   }


makeState :: State
makeState = State { level = l
                  , cubex = newCube l
                  , global = GameGlobal { gamestate = GsStarting
                                        , fade = 1.0
                                        , levelindex = 0
                                        }
                  }
  where
    l = Maps.maps !! 0


nextLevel :: State -> Maybe State
nextLevel state =
  if li < length Maps.maps
    then Just state { level = l
                    , cubex = newCube l
                    , global = GameGlobal { gamestate = GsStarting
                                          , fade = 1.0
                                          , levelindex = li
                                          }
                    }
    else Nothing
  where
    li = (levelindex $ global state) + 1
    l = Maps.maps !! li


-- GL Stuff
data GLStuff = GLStuff {
  texCube  :: GL.TextureObject,
  texFloor :: GL.TextureObject,
  texBg    :: GL.TextureObject
}

initGL :: IO GLStuff
initGL = do
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.depthFunc $= Just GL.Lequal
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.normalize $= GL.Enabled
  GL.texture GL.Texture2D $= GL.Enabled
  
  -- reset matrixes
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity

  -- load textures
  texCube  <- loadTexture "data/cube.tga"
  texFloor <- loadTexture "data/floor.tga"
  texBg    <- loadTexture "data/bg.tga"

  -- make stuff
  return $ GLStuff {
    texCube = texCube,
    texFloor = texFloor,
    texBg = texBg
  }


resize :: GLFW.WindowSizeCallback
resize size@(Size w h) = do
  let hh = if h < 0 then 1 else h
  let aspect = (fromIntegral w) / (fromIntegral hh)
  GL.viewport   $= (Position 0 0, size)
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GLU.perspective 30.0 aspect 1.0 200.0
  GL.matrixMode $= GL.Modelview 0
  return ()


data CubeState = CsOk | CsFall | CsEnd

check :: Maps.Level -> Cube -> CubeState
check level cube =
  if l1 == 'e' && l2 == 'e'
    then CsEnd
    else
      if l1 == ' ' || l2 == ' '
      then CsFall
      else CsOk
  where
    l1 = Maps.getMap level x1 y1
    l2 = Maps.getMap level x2 y2
    x1 = x $ pos1 cube
    y1 = y $ pos1 cube
    x2 = x $ pos2 cube
    y2 = y $ pos2 cube


updateGame :: State -> (GLFW.KeyButtonState,
                        GLFW.KeyButtonState,
                        GLFW.KeyButtonState,
                        GLFW.KeyButtonState) -> GLfloat -> State
updateGame state keys dt =
  if isCubeUpdated cube0 cube1
    then
      state { cubex = cubeNew
            , global = globalNew
            }
    else
      state { cubex = cubeNew }
  where
    cube0 = cubex state
    level0 = level state
    global0 = global state

    cube1 = updateCube dt $ processInput keys cube0
    (cubeNew,globalNew) = case check level0 cube1 of
                            CsOk -> (cube1, global0)
                            CsEnd -> (winCube cube1, global0 { gamestate = GsWinning } )
                            CsFall -> (fallCube cube1, global0 { gamestate = GsFalling } )
              
    processInput (u,d,l,r) cube = head $ [act | (GLFW.Press,act) <- [(u, upCube cube),
                                                                     (d, downCube cube),
                                                                     (l, leftCube cube),
                                                                     (r, rightCube cube)]
                                         ] ++ [cube] -- default: no change

update :: State -> GLfloat -> IO State
update state dt =
  case gamestate global0 of
    GsStarting -> if fade0 < 0.0
                    then
                      return state { global = global0 { gamestate = GsGame
                                                      , fade = 0.0
                                                      }
                                   }
                    else
                      return state { global = global0 { fade = fade0 - 2.0*dt } }
    GsGame -> do
      u <- GLFW.getKey GLFW.UP
      d <- GLFW.getKey GLFW.DOWN
      l <- GLFW.getKey GLFW.LEFT
      r <- GLFW.getKey GLFW.RIGHT
      return $ updateGame state (u,d,l,r) dt
    GsFalling -> if fade0 > 1.0
                   then
                     return state { global = global0 { gamestate = GsStarting
                                                     , fade = 1.0
                                                     }
                                  , cubex = newCube level0
                                  }
                   else
                     return state { global = global0 { fade = fade0 + 2.0*dt }
                                  , cubex = updateCube dt cube0
                                  }
    GsWinning -> if fade0 > 1.0
                   then
                     case nextLevel state of
                       Just newState -> return newState
                       Nothing -> do
                         GLFW.closeWindow
                         return state
                   else
                     return state { global = global0 { fade = fade0 + 2.0*dt }
                                  , cubex = updateCube dt cube0
                                  }
  where
    global0 = global state
    fade0 = fade global0
    level0 = level state
    cube0 = cubex state


renderLevel :: Maps.Level -> IO ()
renderLevel level = do
  forM_ (zip (Maps.lMap level) [0..]) $ \(row,i) ->
    forM_ (zip row [0..]) $ \(c,j) ->
      case c of
        'b' -> GL.preservingMatrix $ do
                 GL.scale (1.0 :: GLfloat) 0.25 1.0
                 GL.translate $ (GL.Vector3 (j*2) (-1) (i*2) :: GL.Vector3 GL.GLfloat)
                 cube
        _ -> return ()

        
render :: State -> GLStuff -> IO ()
render state stuff = do
  GL.clear [GL.ColorBuffer, GL.DepthBuffer]
  GL.loadIdentity

  GLU.lookAt (GL.Vertex3 12.5 30.0 40.0) (GL.Vertex3 15.0 0.0 10.0) (GL.Vector3 0.0 1.0 0.0)

  -- level
  GL.textureBinding GL.Texture2D $= Just (texFloor stuff)
  renderLevel $ level state

  -- cube
  GL.textureBinding GL.Texture2D $= Just (texCube stuff)
  renderCube $ cubex state

  -- 2d
  GL.loadIdentity -- modelview
  GL.matrixMode $= GL.Projection
  GL.preservingMatrix $ do
    -- background
    GL.loadIdentity -- projection
    GL.textureBinding GL.Texture2D $= Just (texBg stuff)
    quad 1.0

    -- fade
    if (gamestate $ global state) /= GsGame
      then do
        GL.texture GL.Texture2D $= GL.Disabled
        GL.blend $= GL.Enabled
        GL.currentColor $= (GL.Color4 0 0 0 (fade $ global state) :: GL.Color4 GL.GLfloat)
        quad 0.0
        GL.currentColor $= (GL.Color4 1 1 1 1 :: Color4 GLfloat) 
        GL.blend $= GL.Disabled
        GL.texture GL.Texture2D $= GL.Enabled
      else
        return ()        

  GL.matrixMode $= GL.Modelview 0

  GLFW.swapBuffers


loop :: State -> GLStuff -> Float -> IO ()
loop state stuff lastTime = do
  -- dt
  nowD <- get time
  let now = double2float nowD
  let dt = realToFrac $ now - lastTime

  -- game
  newState <- update state dt
  render newState stuff

  -- exit if window closed or Esc pressed
  esc <- GLFW.getKey GLFW.ESC
  q <- GLFW.getKey 'Q'
  open <- get $ GLFW.windowParam GLFW.Opened
  if open == 1 && esc /= GLFW.Press && q /= GLFW.Press
    then loop newState stuff now
    else return ()


main :: IO ()
main = do
  -- initialize
  GLFW.initialize
  -- open window
  GLFW.openWindow (GL.Size 700 400) [GLFW.DisplayRGBBits 8 8 8,
                                     GLFW.DisplayAlphaBits 8,
                                     GLFW.DisplayDepthBits 24] GLFW.Window
  -- init
  let state = makeState
  stuff <- initGL
  -- setup stuff
  GLFW.swapInterval       $= 1 -- vsync
  GLFW.windowTitle        $= "Bloxorz"
  GLFW.windowSizeCallback $= resize
  -- main loop
  now <- get GLFW.time
  loop state stuff (double2float now)
  -- exit
  GLFW.closeWindow
  GLFW.terminate
