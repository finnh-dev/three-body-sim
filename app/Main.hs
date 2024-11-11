module Main where
import Graphics.Gloss
import Parts.Init
import Parts.Render

main :: IO ()
main = simulate window background fps initialConditions render update
    where window = InWindow "three-body-sim" (800, 600) (50, 50)
          background = black
          fps = 600
