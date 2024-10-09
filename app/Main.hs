module Main where

import Graphics.Gloss

gConst :: Float
gConst = 1 -- 6.673 * (10.0 ** (-11))

simulationScale :: Float
simulationScale = 100

massScale :: Float
massScale = 1

timeFactor :: Float
timeFactor = 1

tailLength :: Int
tailLength = 300

fadeSpeed :: Float
fadeSpeed = 1 / fromIntegral tailLength

initFromTupels :: SimulationData
initFromTupels = initialConditions ((0,0), (0,1), (0, 5)) ((-0.5, 0.5), (0.5,-0.5), (0,0)) (1,1,0.01)

initialConditions :: ((Float, Float), (Float, Float), (Float, Float)) -> ((Float, Float), (Float, Float), (Float, Float)) -> (Float, Float, Float) -> SimulationData
initialConditions ((p1x, p1y), (p2x, p2y), (p3x, p3y)) ((v1x, v1y), (v2x, v2y), (v3x, v3y)) (mass1, mass2, mass3) = result 
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

fade :: Color -> Color
fade col = makeColor r g b (a - fadeSpeed)
    where
        (r,g,b,a) = rgbaOfColor col

renderTrace :: Color -> [Vector2D] -> [Picture]
renderTrace _ [] = []
renderTrace col (pos:traceBuf) = color col (translate (x pos * simulationScale) (y pos * simulationScale) (circleSolid 1)) : renderTrace (fade col) traceBuf

renderBody :: Body -> Picture
renderBody (Body _ pos _ col traceBuf) =  pictures (renderTrace col traceBuf ++ [color col (translate (x pos * simulationScale) (y pos * simulationScale) (circleSolid 5))])

render :: SimulationData -> Picture
render (SimulationData b1 b2 b3) = pictures [renderBody b1, renderBody b2, renderBody b3]


main :: IO ()
main = simulate window background fps initFromTupels render update
    where window = InWindow "three-body-sim" (800, 600) (50, 50)
          background = black
          fps = 600