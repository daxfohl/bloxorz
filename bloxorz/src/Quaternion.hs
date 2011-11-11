module Quaternion
where

import Graphics.Rendering.OpenGL.GL


type Quaternion = (GLfloat, GLfloat, GLfloat, GLfloat) -- w x y z

quatIdentity :: Quaternion
quatIdentity = (1.0, 0.0, 0.0, 0.0)

quatFromAngleAxis :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> Quaternion
quatFromAngleAxis angle ax ay az = (w, x, y, z)
  where
    w = cos half
    x = hsin * ax
    y = hsin * ay
    z = hsin * az
    half = angle / 2.0
    hsin = sin half


quatMul :: Quaternion -> Quaternion -> Quaternion
quatMul (w1,x1,y1,z1) (w2,x2,y2,z2) = (ww,xx,yy,zz)
  where
    ww = w1*w2 - x1*x2 - y1*y2 - z1*z2
    xx = w1*x2 + x1*w2 + y1*z2 - z1*y2
    yy = w1*y2 + y1*w2 + z1*x2 - x1*z2
    zz = w1*z2 + z1*w2 + x1*y2 - y1*x2


quat2Matrix :: Quaternion -> IO (GLmatrix GLfloat)
quat2Matrix (w,x,y,z) =
  newMatrix ColumnMajor [(r00 :: GLfloat),r01,r02,r03,
                         r10,r11,r12,r13,
                         r20,r21,r22,r23,
                         r30,r31,r32,r33]
  where
    r00 = 1.0 - 2.0*y*y - 2.0*z*z
    r01 = 2.0*x*y + 2.0*w*z
    r02 = 2.0*x*z - 2.0*w*y
    r03 = 0.0

    r10 = 2.0*x*y - 2.0*w*z
    r11 = 1.0 - 2.0*x*x - 2.0*z*z
    r12 = 2.0*y*z + 2.0*w*x
    r13 = 0.0

    r20 = 2.0*x*z + 2.0*w*y
    r21 = 2.0*y*z - 2.0*w*x
    r22 = 1.0 - 2.0*x*x - 2.0*y*y
    r23 = 0.0

    r30 = 0.0
    r31 = 0.0
    r32 = 0.0
    r33 = 1.0
