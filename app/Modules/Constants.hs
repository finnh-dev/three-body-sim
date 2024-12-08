-- | Module: Modules.Constants
-- This module defines constants used in a simulation, such as physical constants,
-- simulation scaling factors, and other parameters.

module Modules.Constants where

-- | Gravitational constant (G).
-- This value is currently set to 1 for simplicity, rather than the actual
-- gravitational constant (approximately 6.673 × 10⁻¹¹ N·(m²/kg²)).
-- Adjust this value as needed for simulations requiring realistic gravitational calculations.
gConst :: Float
gConst = 1 -- 6.673 * (10.0 ** (-11))

-- | Simulation scale factor.
-- This factor determines the scaling of distances in the simulation.
-- A larger value compresses distances, making it easier to visualize simulations
-- of astronomical or large-scale physical systems.
simulationScale :: Float
simulationScale = 50

-- | Time factor for simulation updates.
-- This factor controls the speed of time progression in the simulation.
-- A value of 1 indicates real-time progression, while other values can
-- accelerate or decelerate the simulation's perceived time flow.
timeFactor :: Float
timeFactor = 1

-- | Tail length for visualization.
-- This specifies the number of previous positions (or time steps) to store for
-- visualizing the trajectory of objects in the simulation.
tailLength :: Int
tailLength = 600