-- | Module: Modules.Maths
-- This module provides mathematical utilities for simulating the motion and interactions of
-- celestial bodies in a 2D space. It includes functions for updating positions, applying forces,
-- and computing accelerations based on gravitational interactions.

module Modules.Maths where

import Data.Fixed (mod')
import Modules.Dataclasses
import Modules.Vector2D
import Modules.Constants

-- | Sets a new position for a given `Body`.
-- 
-- @param self The body to update.
-- @param pos The new position as a `Vector2D`.
-- @return A new `Body` with the updated position.
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

-- | Applies acceleration to a body over a given time step, updating its velocity and position.
-- 
-- @param body The body to update.
-- @param acc The acceleration to apply as a `Vector2D`.
-- @param deltaT The time step over which the acceleration is applied.
-- @return A new `Body` with updated velocity, position, and trace buffer.
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

-- | Computes the gravitational acceleration acting on three bodies due to each other.
-- 
-- @param b1 The first body.
-- @param b2 The second body.
-- @param b3 The third body.
-- @return A tuple of accelerations for each body as `Vector2D`.
threeBodyAcceleration :: Body -> Body -> Body -> (Vector2D, Vector2D, Vector2D)
threeBodyAcceleration (Body m1 p1 _ _ _) (Body m2 p2 _ _ _) (Body m3 p3 _ _ _) = result
    where
        result = (
                ((-gConst * m2) `scalarMul` newtonDiv p1 p2) + ((-gConst * m3) `scalarMul` newtonDiv p1 p3),
                ((-gConst * m1) `scalarMul` newtonDiv p2 p1) + ((-gConst * m3) `scalarMul` newtonDiv p2 p3),
                ((-gConst * m1) `scalarMul` newtonDiv p3 p1) + ((-gConst * m2) `scalarMul` newtonDiv p3 p2)
            )

-- | Divides a vector by a scalar value.
-- 
-- @param vec The vector to divide.
-- @param s The scalar divisor.
-- @return The resulting `Vector2D`.
scalarDiv :: Vector2D -> Float -> Vector2D
scalarDiv (Vector2D x1 y1) s = Vector2D (x1 / s) (y1 / s)

-- | Multiplies a scalar value with a vector.
-- 
-- @param s The scalar multiplier.
-- @param vec The vector to scale.
-- @return The resulting `Vector2D`.
scalarMul :: Float -> Vector2D -> Vector2D
scalarMul s (Vector2D x1 y1) = Vector2D (x1 * s) (y1 * s)

-- | Computes the modulo operation on each component of a vector.
-- 
-- @param vec The vector to compute the modulo for.
-- @param s The scalar divisor for the modulo operation.
-- @return The resulting `Vector2D`.
scalarMod :: Vector2D -> Float -> Vector2D
scalarMod (Vector2D x1 y1) s = Vector2D (x1 `mod'` s) (y1 `mod'` s)

-- | Computes the magnitude (length) of a vector.
-- 
-- @param vec The vector to calculate the magnitude of.
-- @return The magnitude as a `Float`.
magnitude :: Vector2D -> Float
magnitude (Vector2D x1 y1) = sqrt (x1 ^ (2 :: Integer) + y1 ^ (2 :: Integer))

-- | Computes the unit vector pointing from one position to another, scaled by the inverse cube of their distance.
-- 
-- @param pa The position of the first body.
-- @param pb The position of the second body.
-- @return The resulting `Vector2D`.
newtonDiv :: Vector2D -> Vector2D -> Vector2D
newtonDiv pa pb = (pa - pb) `scalarDiv` (magnitude (pa - pb) ^ (3 :: Integer))
