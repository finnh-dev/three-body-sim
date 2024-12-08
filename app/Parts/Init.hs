-- | Module: Parts.Init
-- This module defines functions and data for initializing simulation states, including
-- predefined stable orbits and custom initial configurations for celestial bodies.

module Parts.Init where

import Graphics.Gloss
import Parts.Dataclasses
import Parts.Vector2D

-- | The initial conditions for the simulation.
-- This can be set to a specific orbit configuration using `initOrbit` or customized directly.
initialConditions :: SimulationData
-- Uncomment the line below to initialize using a custom array of values.
-- initialConditions = initFromArray [-1, 0, 0.7001954713173643, 0.4071718530521058, 1, 0, 0.7001954713173643, 0.4071718530521058, 0, 0, -1.4003909426347285, -0.8143437061042116] (1 / gConst, 1 / gConst, 1 / gConst)
initialConditions = initOrbit I_B_1_i_c_0_5

-- | Enumeration of predefined stable orbit configurations.
data StableOrbits =
      FigureEight     -- ^ Represents a "Figure 8" orbit (e.g., V.1.A).
    | Flower          -- ^ Represents a "Flower" orbit (e.g., Broucke A2).
    | I_A_1_i_c_0_5   -- ^ Represents an unstable orbit I.A.1 (initial condition 0.5).
    | I_B_1_i_c_0_5   -- ^ Represents an unstable orbit I.B.1 (initial condition 0.5).
    | SetOne_1        -- ^ Represents an unstable orbit from "Set One -> 1".

-- | Initializes a predefined stable orbit based on the given configuration.
initOrbit :: StableOrbits -> SimulationData
initOrbit FigureEight = initFromArray [-1, 0, 0.347113, 0.532727, 1, 0, 0.347113, 0.532727, 0, 0, -0.694226, -1.065454] (1, 1, 1)
initOrbit Flower = initFromArray [0.336130095, 0, 0, 1.532431537, 0.7699893804, 0, 0, -0.6287350978, -1.1061194753, 0, 0, -0.9036964391] (1, 1, 1)
initOrbit I_A_1_i_c_0_5 = initFromArray [-1, 0, 0.2869236336, 0.0791847624, 1, 0, 0.2869236336, 0.0791847624, 0, 0, -1.1476945344, -0.3167390496] (1, 1, 0.5)
initOrbit I_B_1_i_c_0_5 = initFromArray [-1, 0, 0.2374365149, 0.2536896353, 1, 0, 0.2374365149, 0.2536896353, 0, 0, -0.9497460596, -1.0147585412] (1, 1, 0.5)
initOrbit SetOne_1 = initFromArray [-1, 0, 0.7001954713173643, 0.4071718530521058, 1, 0, 0.7001954713173643, 0.4071718530521058, 0, 0, -1.4003909426347285, -0.8143437061042116] (1, 1, 1)

-- | Initializes a `SimulationData` object from an array of position and velocity values.
-- 
-- Used to import arrays in json format from https://observablehq.com/@rreusser/periodic-planar-three-body-orbits
-- Each body is defined using four consecutive elements:
-- - Two for the position (x, y).
-- - Two for the velocity (vx, vy).
-- 
-- @param values A list of 12 floats specifying the positions and velocities of three bodies.
-- @param masses A tuple of three masses for the bodies.
initFromArray :: [Float] -> (Float, Float, Float) -> SimulationData
initFromArray [a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11] (mass1, mass2, mass3) = result
    where
        pos1 = Vector2D a0 a1
        vel1 = Vector2D a2 a3
        pos2 = Vector2D a4 a5
        vel2 = Vector2D a6 a7
        pos3 = Vector2D a8 a9
        vel3 = Vector2D a10 a11
        result = SimulationData {
            body1 = Body {
                mass = mass1,
                position = pos1,
                velocity = vel1,
                c = red,
                traceBuffer = []
            },
            body2 = Body {
                mass = mass2,
                position = pos2,
                velocity = vel2,
                c = green,
                traceBuffer = []
            },
            body3 = Body {
                mass = mass3,
                position = pos3,
                velocity = vel3,
                c = blue,
                traceBuffer = []
            }
        }
initFromArray _ _ = error "Malformed initial conditions"

-- | Initializes a `SimulationData` object from tuples of positions and velocities.
--
-- @param positions A tuple of three 2D positions for the bodies.
-- @param velocities A tuple of three 2D velocities for the bodies.
-- @param masses A tuple of three masses for the bodies.
initFromTupels :: ((Float, Float), (Float, Float), (Float, Float)) 
               -> ((Float, Float), (Float, Float), (Float, Float)) 
               -> (Float, Float, Float) 
               -> SimulationData
initFromTupels ((p1x, p1y), (p2x, p2y), (p3x, p3y)) ((v1x, v1y), (v2x, v2y), (v3x, v3y)) (mass1, mass2, mass3) = result
    where
        pos1 = Vector2D p1x p1y
        pos2 = Vector2D p2x p2y
        pos3 = Vector2D p3x p3y
        vel1 = Vector2D v1x v1y
        vel2 = Vector2D v2x v2y
        vel3 = Vector2D v3x v3y
        result = SimulationData {
            body1 = Body {
                mass = mass1,
                position = pos1,
                velocity = vel1,
                c = red,
                traceBuffer = []
            },
            body2 = Body {
                mass = mass2,
                position = pos2,
                velocity = vel2,
                c = green,
                traceBuffer = []
            },
            body3 = Body {
                mass = mass3,
                position = pos3,
                velocity = vel3,
                c = blue,
                traceBuffer = []
            }
        }