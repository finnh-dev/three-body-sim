-- | Module: Main
-- The entry point for the three-body simulation program.
-- This module uses the Gloss library to simulate and render the motion of three bodies under gravity.

module Main where

import Graphics.Gloss
import Parts.Init
import Parts.Render

-- | The `main` function initializes and runs the simulation using Gloss's `simulate` function.
-- It sets up the simulation window, background, frame rate, initial conditions, rendering logic, 
-- and update logic.

main :: IO ()
main = simulate window background fps initialConditions render update
    where
        -- | The simulation window configuration.
        window = InWindow "three-body-sim" (800, 600) (50, 50)
        
        -- | The background color of the simulation.
        background = black
        
        -- | Frames per second for the simulation.
        fps = 600
