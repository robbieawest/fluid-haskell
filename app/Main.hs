module Main where

import Prelude hiding (replicate, head, tail)
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Data.Vector hiding(map)
import Data.Matrix

import Base
import Simulation
import Render
import Interact


main :: IO ()
main = simulate
            (InWindow "Euler Fluid Solver"
                        (900, 900)
                        (10, 10))
            black
            144
            (initialStatus 20 20)
            (draw True)
            step

step :: ViewPort -> Float -> Status -> Status
--step view dt status = advectDensities (advectVelocities (
 --                       project (modifyVelocities dt (addVelocity status)) 1 dt) dt ) dt

--step view dt status = advectVelocities (extrapolate (project (clearPressure status) 20 dt)) dt
step view dt status = project status 20 dt

--step view dt status = project(modifyVelocities dt(addVelocity ))