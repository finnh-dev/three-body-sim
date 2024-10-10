module Main where

import Graphics.Gloss
import Data.Fixed (mod')

gConst :: Float
gConst = 1 -- 6.673 * (10.0 ** (-11))

simulationScale :: Float
simulationScale = 100

massScale :: Float
massScale = 1

timeFactor :: Float
timeFactor = 0.5

tailLength :: Int
tailLength = 1000

fadeSpeed :: Float
-- fadeSpeed = 0
fadeSpeed = 1 / fromIntegral tailLength

initialConditions :: SimulationData
-- initialConditions = initFromTupels ((-1,0), (1,0), (0, 0)) ((0.557809,0.451774), (0.557809,0.451774), (-1.115618,-0.903548)) (1, 1, 1)
-- initialConditions = initFromTupels
--     ((-1.1889693067,0),
--     (3.8201881837,0),
--     (-2.631218877,0))

--     ((0, 0.8042120498),
--     (0, 0.0212794833),
--     (0, -0.8254915331))

--     (1, 1, 1) 
initialConditions = initFromArray  [0.898348747, 0, 0, 0.9475564971, -0.6754911045, 0, 0, -1.7005860354, -0.2228576425, 0, 0, 0.7530295383] (1,1,1)

initFromArray :: [Float] -> (Float, Float, Float) -> SimulationData
initFromArray a (mass1, mass2, mass3) = result
    where
        pos1 = Vector2D (a!!0) (a!!1)
        vel1 = Vector2D (a!!2) (a!!3)
        pos2 = Vector2D (a!!4) (a!!5)
        vel2 = Vector2D (a!!6) (a!!7)
        pos3 = Vector2D (a!!8) (a!!9)
        vel3 = Vector2D (a!!10) (a!!11)
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

initFromTupels :: ((Float, Float), (Float, Float), (Float, Float)) -> ((Float, Float), (Float, Float), (Float, Float)) -> (Float, Float, Float) -> SimulationData
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

data Vector2D = Vector2D {
    x :: Float,
    y :: Float
} deriving Show


instance Num Vector2D where
    (+) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 + x2) (y1 + y2)
    (-) (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 - x2) (y1 - y2)
    (*) = error "Unimplemented"
    negate _ = error "Unimplemented"
    abs = error "Unimplemented"
    signum = error "Unimplemented"
    fromInteger = error "Unimplemented"

data Body = Body {
    mass :: Float,
    position :: Vector2D,
    velocity :: Vector2D,
    c :: Color,
    traceBuffer :: [Vector2D]
} deriving Show

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
data SimulationData = SimulationData {
    body1 :: Body,
    body2 :: Body,
    body3 :: Body
} deriving Show

initData :: SimulationData
initData = SimulationData {
    body1 = Body {
        mass = 1 * massScale,
        position = Vector2D {
            x = -0.97000436,
            y = 0.24308753
        },
        velocity = Vector2D {
            x = 0.4662036850,
            y = 0.4323657300
        },
        c = green,
        traceBuffer = []
    },
    body2 = Body {
        mass = 1 * massScale,
        position = Vector2D {
            x = 0,
            y = 0
        },
        velocity = Vector2D {
            x = -0.93240737,
            y = -0.86473146
        },
        c = red,
        traceBuffer = []
    },
    body3 = Body {
        mass = 1 * massScale,
        position = Vector2D {
            x = 0.97000436,
            y = -0.24308753
        },
        velocity = Vector2D {
            x = 0.4662036850,
            y = 0.4323657300
        },
        c = blue,
        traceBuffer = []
    }
}


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

threeBodyAcceleration :: Body -> Body -> Body -> (Vector2D, Vector2D, Vector2D)
threeBodyAcceleration (Body m1 p1 _ _ _) (Body m2 p2 _ _ _) (Body m3 p3 _ _ _) = result
    where
        result = (
                ((-gConst * m2) `scalarMul`  newtonDiv p1 p2) + ((-gConst * m3) `scalarMul`  newtonDiv p1 p3),
                ((-gConst * m1) `scalarMul`  newtonDiv p2 p1) + ((-gConst * m3) `scalarMul`  newtonDiv p2 p3),
                ((-gConst * m1) `scalarMul`  newtonDiv p3 p1) + ((-gConst * m2) `scalarMul`  newtonDiv p3 p2)
            )

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

update :: p1 -> Float -> SimulationData -> SimulationData
update _ deltaT simData = newSimData
    where
        (a1, a2, a3) = threeBodyAcceleration (body1 simData) (body2 simData) (body3 simData)
        newSimData = SimulationData {
            body1 = applyAcceleration (body1 simData) a1 deltaT,
            body2 = applyAcceleration (body2 simData) a2 deltaT,
            body3 = applyAcceleration (body3 simData) a3 deltaT
        }

transformTails :: Body -> Vector2D -> Body
transformTails self center = result
    where
        result = Body {
            mass = mass self,
            position = position self,
            velocity = velocity self,
            c = c self,
            traceBuffer = map (\pos -> pos - center) (traceBuffer self)
        }

transformPositions :: Vector2D -> SimulationData -> SimulationData
transformPositions center simData = result
    where
        result = SimulationData {
            body1 = transformTails (setPosition (body1 simData) (position (body1 simData) - center)) center,
            body2 = transformTails (setPosition (body2 simData) (position (body2 simData) - center)) center,
            body3 = transformTails (setPosition (body3 simData) (position (body3 simData) - center)) center
        }

fade :: Color -> Color
fade col = makeColor r g b (a - fadeSpeed)
    where
        (r,g,b,a) = rgbaOfColor col

renderTrace :: Color -> [Vector2D] -> [Picture]
renderTrace _ [] = []
renderTrace col (pos:traceBuf) = color col (translate (x pos * simulationScale) (y pos * simulationScale) (circleSolid 1)) : renderTrace (fade col) traceBuf

renderBody :: Body -> Picture
renderBody (Body _ pos _ col traceBuf) =  pictures (renderTrace col traceBuf ++ [color col (translate (x pos * simulationScale) (y pos * simulationScale) (circleSolid 5))])

renderBodies :: SimulationData -> Picture
renderBodies (SimulationData b1 b2 b3) = pictures [renderBody b1, renderBody b2, renderBody b3]

render :: SimulationData -> Picture
render simData = pictures (renderGrid center ++ [renderBodies (transformPositions center simData)])
    where
        --center = position (body2 simData)
        center = (position (body1 simData) + position (body2 simData) + position (body3 simData)) `scalarDiv` 3

drawGrid :: Vector2D -> Float -> Float -> Float -> Color -> [Picture]
drawGrid (Vector2D cx cy) width height spacing col =
    let verticalLines = [Line [(xpos - offsetx,-height / 2 - offsety), (xpos - offsetx, height / 2 - offsety)] | xpos <- [(-width / 2), (-width / 2 + spacing)..(width / 2)]]
        horizontalLines = [Line [(-width / 2 - offsetx, ypos - offsety), (width / 2 - offsetx, ypos - offsety)] | ypos <- [(-height / 2), (-height / 2 + spacing)..(height / 2)]]
    in map (Color col) (verticalLines ++ horizontalLines)
    where
        offsetx = (cx * simulationScale) `mod'` spacing
        offsety = (cy * simulationScale) `mod'` spacing

renderGrid :: Vector2D -> [Picture]
renderGrid center = drawGrid center 20000 20000 40 (greyN 0.1)

main :: IO ()
main = simulate window background fps initialConditions render update
    where window = InWindow "three-body-sim" (800, 600) (50, 50)
          background = black
          fps = 600