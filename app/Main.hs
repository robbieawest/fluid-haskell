module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort


main :: IO ()
main = simulate
            (InWindow "Euler Fluid Solver"
                        (900, 900)
                        (10, 10))
            black
            144
            initialStatus
            draw
            step

type Matrix a = [[a]]
data Particle = Particle {
    density :: Float,
    velocity :: Vector, --Vector is just Point, Point :: (Float, Float)
    pressure :: Float
}

data Status = Status {
    particles :: Matrix Particle, --This may seem wierd as this is an Euler fluid solve, but the particles do not really move
    boxSize :: Vector,
    len :: Int, --Where 1 is 1 box grid
    width :: Int
}

initialStatus :: status 
initialStatus = undefined

draw :: Status -> Picture
draw = undefined

step :: ViewPort -> Float -> Status -> Status
step = undefined

