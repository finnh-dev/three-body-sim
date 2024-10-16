module Main where

import Graphics.Gloss
import Data.Fixed (mod')

gConst :: Float
gConst = 1 -- 6.673 * (10.0 ** (-11))

simulationScale :: Float
simulationScale = 50

massScale :: Float
massScale = 1

timeFactor :: Float
timeFactor = 1

tailLength :: Int
tailLength = 600

fadeSpeed :: Float
-- fadeSpeed = 0
fadeSpeed = 1 / fromIntegral tailLength

initialConditions :: SimulationData
initialConditions = initOrbit SetOne_1

data StableOrbits =
    FigureEight     -- V Figure 8 -> V.1.A
    | Flower        -- Broucke -> Broucke A2
    | I_A_1_i_c_0_5 -- I.A i.c. -> I.A.1 i.c. (0.5) // unstable
    | I_B_1_i_c_0_5 -- I.B i.c. -> I.B.1 i.c. (0.5) // unstable
    | SetOne_1      -- Set One -> 1 // unstable


initOrbit :: StableOrbits -> SimulationData
initOrbit FigureEight = initFromArray [-1, 0, 0.347113, 0.532727, 1, 0, 0.347113, 0.532727, 0, 0, -0.694226, -1.065454] (1, 1, 1)
initOrbit Flower = initFromArray [0.336130095, 0, 0, 1.532431537, 0.7699893804, 0, 0, -0.6287350978, -1.1061194753, 0, 0, -0.9036964391] (1, 1, 1)
initOrbit I_A_1_i_c_0_5 = initFromArray [-1, 0, 0.2869236336, 0.0791847624, 1, 0, 0.2869236336, 0.0791847624, 0, 0, -1.1476945344, -0.3167390496] (1, 1, 0.5)
initOrbit I_B_1_i_c_0_5 = initFromArray [-1, 0, 0.2374365149, 0.2536896353, 1, 0, 0.2374365149, 0.2536896353, 0, 0, -0.9497460596, -1.0147585412] (1, 1, 0.5)
initOrbit SetOne_1 = initFromArray [-1, 0, 0.7001954713173643, 0.4071718530521058, 1, 0, 0.7001954713173643, 0.4071718530521058, 0, 0, -1.4003909426347285, -0.8143437061042116] (1, 1, 1)

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
render simData = pictures (renderGrid center ++ [renderBodies transformedSimData, drawCOGeoTriangle transformedSimData])
    where
        -- center = position (body2 simData)
        center = (position (body1 simData) + position (body2 simData) + position (body3 simData)) `scalarDiv` 3
        transformedSimData = transformPositions center simData

drawGrid :: Vector2D -> Float -> Float -> Float -> Color -> [Picture]
drawGrid (Vector2D cx cy) width height spacing col =
    let verticalLines = [Line [(xpos - offsetx,-height / 2 - offsety), (xpos - offsetx, height / 2 - offsety)] | xpos <- [(-width / 2), (-width / 2 + spacing)..(width / 2)]]
        horizontalLines = [Line [(-width / 2 - offsetx, ypos - offsety), (width / 2 - offsetx, ypos - offsety)] | ypos <- [(-height / 2), (-height / 2 + spacing)..(height / 2)]]
    in map (Color col) (verticalLines ++ horizontalLines)
    where
        offsetx = (cx * simulationScale) `mod'` spacing
        offsety = (cy * simulationScale) `mod'` spacing

drawCOGeoTriangle :: SimulationData -> Picture
drawCOGeoTriangle (SimulationData b1 b2 b3) = color (greyN 0.5) (pictures
    [
        line [(x1, y1), (x2, y2), (x3, y3), (x1, y1)],
        line [(x1/2 + x2/2, y1/2 + y2/2), (x3, y3)],
        line [(x1/2 + x3/2, y1/2 + y3/2), (x2, y2)],
        line [(x2/2 + x3/2, y2/2 + y3/2), (x1, y1)],
        translate xCOGeo yCOGeo (circleSolid 2)
    ])
    where
        (x1, y1) = (x (position b1) * simulationScale, y (position b1) * simulationScale)
        (x2, y2) = (x (position b2) * simulationScale, y (position b2) * simulationScale)
        (x3, y3) = (x (position b3) * simulationScale, y (position b3) * simulationScale)
        (xCOGeo, yCOGeo) =  ((x1 + x2 + x3) / 3, (y1 + y2 + y3) / 3)

renderGrid :: Vector2D -> [Picture]
renderGrid center = drawGrid center 20000 20000 40 (greyN 0.1)

main :: IO ()
main = simulate window background fps initialConditions render update
    where window = InWindow "three-body-sim" (800, 600) (50, 50)
          background = black
          fps = 600