module Cubezor where

import Graphics.Rendering.OpenGL
import Etc
import Maps
import Quaternion


-- Cube types
data CubePos = CubePos {
  x :: Int,
  y :: Int
} deriving Show

data CubeOrientation = Standing | Horizontal | Vertical deriving Eq

data CubeRolling = CrNo | CrUp | CrDown | CrRight | CrLeft | CrFalling | CrWinning deriving (Eq, Show)

data Cube = Cube { pos1      :: CubePos
                 , pos2      :: CubePos
                 , angle     :: GLfloat
                 , rolling   :: CubeRolling
                 , quat      :: Quaternion
                 , rotOffset :: Vector3 GLfloat
                 , rotAxis   :: Vector3 GLfloat
                 , fallOffset :: GLfloat
                 } deriving Show


cubeRollingSpeed :: GLfloat
cubeRollingSpeed = 360.0


newCube :: Maps.Level -> Cube
newCube level = Cube {
    pos1  = p,
    pos2  = p,
    angle = 0.0,
    rolling = CrNo,
    quat = quatIdentity,
    rotOffset = (Vector3 0.0 0.0 0.0 :: Vector3 GLfloat),
    rotAxis = (Vector3 0.0 0.0 0.0 :: Vector3 GLfloat),
    fallOffset = 0.0
  } where
    (x,y) = lStartPos level
    p = CubePos {x = x, y = y}


renderCube :: Cube -> IO ()
renderCube cubex = preservingMatrix $ do
  let xx = fromIntegral ((x $ pos1 cubex) + (x $ pos2 cubex))
  let yy = fromIntegral ((y $ pos1 cubex) + (y $ pos2 cubex))
  let zz = if orientation cubex == Standing then 2.0 else 1.0

  if (rolling cubex == CrFalling) || (rolling cubex == CrWinning)
    then translate (Vector3 0 (fallOffset cubex) 0 :: Vector3 GLfloat)
    else return ()

  translate $ (Vector3 xx 0 yy :: Vector3 GLfloat)
  if rolling cubex /= CrNo
    then do
      translate $ vecNeg $ rotOffset cubex
      rotate (angle cubex) (rotAxis cubex)
      translate $ rotOffset cubex
    else
      return ()

  translate $ (Vector3 0.0 zz 0.0 :: Vector3 GLfloat)
  m <- quat2Matrix $ quat cubex
  multMatrix m
  scale (1.0 :: GLfloat) 2.0 1.0
  cube


upCube :: Cube -> Cube
upCube cube =
  if rolling cube == CrNo
    then 
      cube { rolling = CrUp
           , rotAxis = Vector3 (-1.0) 0.0 0.0
           , rotOffset = Vector3 0.0 0.0 ro
           }
    else
      cube
    where
      ro = if orientation cube == Vertical
             then 2.0
             else 1.0


downCube :: Cube -> Cube
downCube cube =
  if rolling cube == CrNo
    then
      cube { rolling = CrDown
           , rotAxis = Vector3 1.0 0.0 0.0
           , rotOffset = Vector3 0.0 0.0 ro
           }
    else
      cube
  where
    ro = if orientation cube == Vertical
           then (-2.0)
           else (-1.0)


leftCube :: Cube -> Cube
leftCube cube =
  if rolling cube == CrNo
    then
      cube { rolling = CrLeft
           , rotAxis = Vector3 0.0 0.0 1.0
           , rotOffset = Vector3 ro 0.0 0.0
           }
    else
      cube
  where
    ro = if orientation cube == Horizontal
           then 2.0
           else 1.0


rightCube :: Cube -> Cube
rightCube cube =
  if rolling cube == CrNo
    then
      cube { rolling = CrRight
           , rotAxis = Vector3 0.0 0.0 (-1.0)
           , rotOffset = Vector3 ro 0.0 0.0
           }
    else
      cube
  where
    ro = if orientation cube == Horizontal
           then (-2.0)
           else (-1.0)


fallCube :: Cube -> Cube
fallCube cube = cube { rolling = CrFalling
                     , fallOffset = 0.0
                     }


winCube :: Cube -> Cube
winCube cube = cube { rolling = CrWinning
                    , fallOffset = 0.0
                    }


orientation :: Cube -> CubeOrientation
orientation cube =
  if x1 == x2 && y1 == y2
    then Standing
    else
      if x1 < x2 && y1 == y2
        then Horizontal
        else Vertical
  where
    x1 = x $ pos1 cube
    y1 = y $ pos1 cube
    x2 = x $ pos2 cube
    y2 = y $ pos2 cube


updateCube :: GLfloat -> Cube -> Cube
updateCube dt cube =
  if (angle cube > 90.0) && (rolling cube /= CrFalling) && (rolling cube /= CrWinning)
    then
      cube { pos1 = p1
           , pos2 = p2
           , quat = q
           , angle = 0
           , rolling = CrNo
           }
    else
      if rolling cube /= CrNo
        then cube { angle = a
                  , fallOffset = fo 
                  }
        else cube
  where
    (p1,p2,q) = updatePosition cube
    fo = if (rolling cube == CrFalling) || (rolling cube == CrWinning)
           then fallOffset cube - 10.0 * dt
           else 0.0
    a = if rolling cube == CrWinning
          then 0.0
          else angle cube + cubeRollingSpeed * dt


updatePosition :: Cube -> (CubePos, CubePos, Quaternion)
updatePosition cube =
  case rolling cube of
    CrUp -> case orientation cube of
            Standing   -> ( CubePos  xx      (yy - 2)
                          , CubePos  xx      (yy - 1)
                          , quatMul q qq
                          )
            Horizontal -> ( CubePos  xx      (yy - 1)
                          , CubePos (xx + 1) (yy - 1)
                          , quatMul q qq
                          )
            Vertical   -> ( CubePos  xx      (yy - 1)
                          , CubePos  xx      (yy - 1)
                          , quatMul q qq
                          )
            where q = quatFromAngleAxis piPer2 (-1.0) 0.0 0.0
    CrDown -> case orientation cube of
            Standing   -> ( CubePos  xx      (yy + 1)
                          , CubePos  xx      (yy + 2)
                          , quatMul q qq
                          )
            Horizontal -> ( CubePos  xx      (yy + 1)
                          , CubePos (xx + 1) (yy + 1)
                          , quatMul q qq
                          )
            Vertical   -> ( CubePos  xx      (yy + 2)
                          , CubePos  xx      (yy + 2)
                          , quatMul q qq
                          )
            where q = quatFromAngleAxis piPer2 1.0 0.0 0.0
    CrLeft -> case orientation cube of
            Standing   -> ( CubePos (xx - 2)  yy
                          , CubePos (xx - 1)  yy
                          , quatMul q qq
                          )
            Horizontal -> ( CubePos (xx - 1)  yy
                          , CubePos (xx - 1)  yy
                          , quatMul q qq
                          )
            Vertical   -> ( CubePos (xx - 1)  yy
                          , CubePos (xx - 1) (yy + 1)
                          , quatMul q qq
                          )
            where q = quatFromAngleAxis piPer2 0.0 0.0 1.0
    CrRight -> case orientation cube of
            Standing   -> ( CubePos (xx + 1)  yy
                          , CubePos (xx + 2)  yy
                          , quatMul q qq
                          )
            Horizontal -> ( CubePos (xx + 2)  yy
                          , CubePos (xx + 2)  yy
                          , quatMul q qq
                          )
            Vertical   -> ( CubePos (xx + 1)  yy
                          , CubePos (xx + 1) (yy + 1)
                          , quatMul q qq
                          )
            where q = quatFromAngleAxis piPer2 0.0 0.0 (-1.0)
    -- otherwise just return the old value
    _ -> (pos1 cube, pos2 cube, quat cube)
  where
    xx = x $ pos1 cube
    yy = y $ pos1 cube
    qq = quat cube


isCubeUpdated :: Cube -> Cube -> Bool
isCubeUpdated cube0 cube1 = (angle cube0 > 90.0) && (angle cube1 == 0.0)
