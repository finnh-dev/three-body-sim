module Parts.Render where

import Data.Fixed (mod')
import Graphics.Gloss
import Parts.Dataclasses
import Parts.Vector2D
import Parts.Constants
import Parts.Maths


fadeSpeed :: Float
-- fadeSpeed = 0
fadeSpeed = 1 / fromIntegral tailLength

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
