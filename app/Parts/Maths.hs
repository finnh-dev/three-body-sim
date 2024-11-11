module Parts.Maths where

import Data.Fixed (mod')
import Parts.Dataclasses
import Parts.Vector2D
import Parts.Constants

setPosition :: Body -> Vector2D -> Body
setPosition self pos = result
    where
        result = Body {
            mass = mass self,
            position = pos,
            velocity = velocity self,
            c = c self,
            traceBuffer = traceBuffer self
        }

applyAcceleration :: Body -> Vector2D -> Float -> Body
applyAcceleration body acc deltaT = result
    where
        vel = velocity body + ((deltaT * timeFactor) `scalarMul` acc)
        result = Body {
            mass = mass body,
            position = position body + ((deltaT * timeFactor) `scalarMul` vel),
            velocity = vel,
            c = c body,
            traceBuffer = take tailLength (position body : traceBuffer body)
        }

threeBodyAcceleration :: Body -> Body -> Body -> (Vector2D, Vector2D, Vector2D)
threeBodyAcceleration (Body m1 p1 _ _ _) (Body m2 p2 _ _ _) (Body m3 p3 _ _ _) = result
    where
        result = (
                ((-gConst * m2) `scalarMul`  newtonDiv p1 p2) + ((-gConst * m3) `scalarMul`  newtonDiv p1 p3),
                ((-gConst * m1) `scalarMul`  newtonDiv p2 p1) + ((-gConst * m3) `scalarMul`  newtonDiv p2 p3),
                ((-gConst * m1) `scalarMul`  newtonDiv p3 p1) + ((-gConst * m2) `scalarMul`  newtonDiv p3 p2)
            )

scalarDiv :: Vector2D -> Float -> Vector2D
scalarDiv (Vector2D x1 y1) s = Vector2D (x1 / s) (y1 / s)

scalarMul :: Float -> Vector2D -> Vector2D
scalarMul s (Vector2D x1 y1) = Vector2D (x1 * s) (y1 * s)

scalarMod :: Vector2D -> Float -> Vector2D
scalarMod (Vector2D x1 y1) s = Vector2D (x1 `mod'` s) (y1 `mod'` s)

magnitude :: Vector2D -> Float
magnitude (Vector2D x1 y1) = sqrt (x1 ^ (2 :: Integer) + y1 ^ (2 :: Integer))

newtonDiv :: Vector2D -> Vector2D -> Vector2D
newtonDiv pa pb = (pa - pb) `scalarDiv` (magnitude (pa - pb) ^ (3 :: Integer))
