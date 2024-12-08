-- | Module: Modules.Dataclasses
-- This module defines data structures used in the simulation, such as celestial bodies and
-- the overall simulation state. These structures encapsulate the properties and behaviors
-- of simulated entities.

module Modules.Dataclasses where

import Graphics.Gloss
import Modules.Vector2D

-- | Represents a celestial body or particle in the simulation.
data Body = Body {
    mass :: Float,               -- ^ The mass of the body (in arbitrary units).
    position :: Vector2D,        -- ^ The current position of the body in 2D space.
    velocity :: Vector2D,        -- ^ The current velocity of the body in 2D space.
    c :: Color,                  -- ^ The color of the body for rendering purposes.
    traceBuffer :: [Vector2D]    -- ^ A list of past positions used for visualizing the trajectory.
} deriving Show

-- | Represents the state of the simulation, containing multiple celestial bodies.
data SimulationData = SimulationData {
    body1 :: Body,  -- ^ The first celestial body in the simulation.
    body2 :: Body,  -- ^ The second celestial body in the simulation.
    body3 :: Body   -- ^ The third celestial body in the simulation.
} deriving Show
