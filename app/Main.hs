{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude hiding (replicate, head, tail)
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Data.Strict.Vector as V hiding(map)
--import Data.Vector hiding(map)

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
--step view dt status = project status 20 dt
--step view dt status = extrapolate(project(clearPRessure status) 20 dt)
--step view dt status = extrapolate $! project (clearPressure status) 20 dt
step view dt status = adRes
    where
        adRes = seq extrapRes (advectVelocities extrapRes dt)
        extrapRes = seq projRes (extrapolate projRes)
        projRes = seq r2 project r2 20 dt
        r2 = clearPressure status


--step view dt status = project(modifyVelocities dt(addVelocity ))
--step view dt status = advectDensities (advectVelocities (
 --                       project (modifyVelocities dt (addVelocity status)) 1 dt) dt ) dt

--step view dt status = seq proj (advectVelocities proj dt)
--step view dt status = ext
 --   where
 --       clearP = clearPressure status
 --       --proj = project status 20 dt
  --      proj = seq clearP project clearP 20 dt
  --      ext = seq proj extrapolate proj
        
