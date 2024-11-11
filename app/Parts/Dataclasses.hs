module Parts.Dataclasses where

import Graphics.Gloss
import Parts.Vector2D

data Body = Body {
    mass :: Float,
    position :: Vector2D,
    velocity :: Vector2D,
    c :: Color,
    traceBuffer :: [Vector2D]
} deriving Show

data SimulationData = SimulationData {
    body1 :: Body,
    body2 :: Body,
    body3 :: Body
} deriving Show
