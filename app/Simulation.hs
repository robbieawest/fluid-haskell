module Simulation where

import Data.Vector
import Prelude hiding (replicate, head, tail, map)

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Base

{-
-- Setting Up
addGravity :: Float -> (Int, Int) -> GridEntry -> GridEntry
addGravity dt _ (GridEntry d (x, y) p s)
    | s == 1 = GridEntry d (x, y - 9.8 * dt) p s
    | otherwise = GridEntry d (x, y) p s

modifyVelocities :: Float -> Status -> Status
modifyVelocities dt (Status g b w h) = Status (mapPos (addGravity dt) g) b w h

addVelocity :: Status -> Status 
addVelocity (Status g b w h) = Status (add4 g w h) b w h
    where
        add4 g w h
            | w < 4 || h < 4 = g
            | otherwise = updateMat g [
                ((center h, 2), GridEntry 1.0 (v (center h) 2) (p (center h) 2) (s (center h) 2)),
                ((center h, 3), GridEntry 1.0 (v (center h) 3) (p (center h) 3) (s (center h) 3)),
                ((center h + 1, 2), GridEntry 1.0 (v (center h + 1) 2) (p (center h + 1) 2) (s (center h + 1) 2)),
                ((center h + 1, 3), GridEntry 1.0 (v (center h + 1) 3) (p (center h + 1) 3) (s (center h + 1) 3))
            ]
        center h = h `div` 2
        v i j = getVel (getElem i j g) `addVel` (3.0, 0.0)
        p i j = getPressure (getElem i j g)
        s i j = getS (getElem i j g)





--Advection
--Advection makes things move
--This type of advection is called Semi-Lagrangian Advection, if you look it up, bad things will happen
--We have a grid of "particles", this obviously does not make sense for advecting a fluid, since nothing moves.
--So we get the velocity at a position, move back in time from its position, and then interpolate(weighted average)
--the velocities surrounding the new position, as 99.999% of the time it will not fall upon a "particle".
--This procedure induces viscocity, since the backtracing is linear through the fluid.
--This means that we will have less curls and fancy vortexes ;(, but we can artificially add curl to the vector field
--using Vorticity Confinement, which I may or may not implement into this depending if I have the time :p
--the difference isn't massive really. 

lerp :: Float -> Float -> Float -> Float
--Linear interpolation function
lerp a b k = a + k * (b - a)

getStaggeredPosition :: Float -> Int -> Int -> Vec2f
--This calculates the staggered position of the grid at zero indexes i, j
--Namely the position of the _u::Float component of the _v::Vec2f velocity, as opposed to the _v::Float component
--Where height is the box size (Square so width = height)
getStaggeredPosition height i j = (height * fromIntegral i,
                          height * fromIntegral j + height / 2.0)



advectVelX :: Status -> (Int, Int) -> Vec2f -> Float -> Float
advectVelX (Status grid boxSize w h) (i, j) (u, _) dt = sampleField (Status grid boxSize w h) posx posy U
    where
        b2 = boxSize * 0.5
        x = (fromIntegral i - 1.0) * boxSize 
        y = (fromIntegral j - 1.0) * boxSize + b2
        v = calculateV grid i j --Average y components of velocities surrounding
        (posx, posy) = (x - dt * u, y - dt * v)

advectVelY :: Status -> (Int, Int) -> Vec2f -> Float -> Float
advectVelY (Status grid boxSize w h) (i, j) (_, v) dt = sampleField (Status grid boxSize w h) posx posy V
    where
        b2 = boxSize * 0.5
        x = (fromIntegral i - 1.0) * boxSize + b2
        y = (fromIntegral j - 1.0) * boxSize
        u = calculateU grid i j --Average y components of velocities surrounding
        (posx, posy) = (x - dt * u, y - dt * v)




advectVel :: Vector Float -> Vector Float -> Vector Float -> Float -> (Int, Int) -> Vec2f -> Vector Float
advectVel uField vField sField dt (i, j) (currU, currV)
    | s == 1 = GridEntry d (advectVelX (Status grid boxSize w h) (i, j) currU dt,
                            advectVelY (Status grid boxSize w h) (i, j) currV dt) p s 
    | otherwise = GridEntry d v p s
-}

--Main simulation juice
data Field = Pressure | U | V | Density
    deriving(Eq)

--Set pressure field to new field of zeroes
clearPressure :: Status -> Status
clearPressure (Status u v p d s h numX numY) = Status u v (replicate (numX * numY) 0.0) d s h numX numY

extrapolate :: Status -> Status
extrapolate (Status u v p d s h numX numY) = Status (extraU u numX numY) (extraV v numX numY) p d s h numX numY
    where
        extraU u numX numY = u // iterU 0
        extraV v numX numY = v // iterV 0
        
        iterU iter
            | iter < numX = [
                (iter * numY, indf iter 1 numY u),
                (i2i1 (iter, numY - 1) numY, indf iter (numY - 2) numY u)
                ] Prelude.++ iterU (iter + 1)
            | otherwise = []
        
        iterV iter
            | iter < numY = [
                (iter, indf 1 iter numY v),
                (i2i1 (numX - 1, iter) numY, indf (numX - 2) iter numY v)
                ] Prelude.++ iterV (iter + 1)
            | otherwise = []

--This function interpolates against a position, variable with each different field type
sampleField :: Vector Float -> Float -> Float -> Float -> Int -> Int -> Field -> Float
sampleField field x y h numX numY fieldType = ret
    where
        h1 = 1.0 / h
        h2 = 0.5 * h

        xo = max (min x (fromIntegral numX * h)) h --Constrict x and y to the field(cannot go outside of width and height)
        yo = max (min y (fromIntegral numY * h)) h
        
        (dx, dy)
            | fieldType == U = (0.0, h2)
            | fieldType == V = (h2, 0.0)
            | fieldType == Density = (h2, h2)

        x0 = min (floor ((xo - dx) * h1)) (numX - 1)
        x1 = min (x0 + 1) (numX - 1)
        tx = ((x - dx) - fromIntegral x0 * h) * h1

        y0 = min (floor ((yo - dy) * h1)) (numY - 1)
        y1 = min (y0 + 1) (numY - 1)
        ty = ((y - dy) - fromIntegral y0 * h) * h1

        sx = 1.0 - tx
        sy = 1.0 - ty

        ret = sx * sy * indf x0 y0 numY field + 
              tx * sy * indf x1 y0 numY field +
              tx * ty * indf x1 y1 numY field +
              sx * ty * indf x0 y1 numY field

avgField :: Vector Float -> (Int, Int) -> Int -> Float
--Averages around a field with an index, used in advection mainly
avgField field (i, j) numY = (indf i j numY field + 
                              indf i (j + 1) numY field +
                              indf (i - 1) j numY field +
                              indf (i - 1) (j + 1) numY field) * 0.25


advectU :: Vector Float -> Vector Float -> Vector Float -> Float -> Float -> Int -> Int -> Int -> Float -> Float
advectU uField vField sField dt h numX numY ind currU
    | sField ! ind == 1.0 = sampleField uField posx posy h numX numY U
    | otherwise = currU
    where
        (i, j) = i1i2 ind numY --Convert indexes to 2d

        v = avgField vField (i, j) numY --Get v component by averaging the field
        h2 = h * 0.5

        x = (fromIntegral i - 1.0) * h
        y = (fromIntegral j - 1.0) * h + h2

        (posx, posy) = (x - dt * currU, y - dt * v)

advectV :: Vector Float -> Vector Float -> Vector Float -> Float -> Float -> Int -> Int -> Int -> Float -> Float
advectV vField uField sField dt h numX numY ind currV
    | sField ! ind == 1.0 = sampleField vField posx posy h numX numY V
    | otherwise = currV
    where
        (i, j) = i1i2 ind numY --Convert indexes to 2d

        u = avgField uField (i, j) numY --Get u component by averaging the field
        h2 = h * 0.5

        x = (fromIntegral i - 1.0) * h + h2 
        y = (fromIntegral j - 1.0) * h
        
        (posx, posy) = (x - dt * u, y - dt * currV)
        


advectVelocities :: Status -> Float -> Status --Code uses old field then blalblabla you need to update new vector without editing old
advectVelocities (Status u v p d s h numX numY) dt = Status advectedU advectedV p d s h numX numY
    where 
        advectedU = imap (advectU u v s dt h numX numY) u
        advectedV = imap (advectV v u s dt h numX numY) v

proj :: Status -> Float -> Int -> Int -> Status
proj (Status uField vField pField d sField h numX numY) dt i j
    | indf i j numY sField == 1.0 && s /= 0.0 = Status newUField newVField newPField d sField h numX numY
    | otherwise = Status uField vField pField d sField h numX numY
    where
        --Clean this up later 
        sOrigin = indf i j numY sField
        sx0 = indf (i - 1) j numY sField
        sx1 = indf (i + 1) j numY sField
        sy0 = indf i (j - 1) numY sField
        sy1 = indf i (j + 1) numY sField

        s = sOrigin + sx0 + sx1 + sy0 + sy1

        divergence = indf (i + 1) j numY uField - indf i j numY uField +
                     indf i (j + 1) numY vField - indf i j numY vField

        p = -1.9 * (divergence / s) --1.9 is our overrelaxation constant
        newPField = pField // [(i2i1 (i, j) numY, 1000.0 * p * h / dt)]

        newUField = uField // [
            (i2i1 (i, j) numY, sx0 * p),
            (i2i1 (i + 1, j) numY, sx1 * p)
            ]
        
        newVField = vField // [
            (i2i1 (i, j) numY, sy0 * p),
            (i2i1 (i, j + 1) numY, sy1 * p)
            ]


projectInner :: Status -> Float -> Int -> Int -> Status
projectInner (Status u v p d s h numX numY)  dt i j
    | j < numY - 1 = projectInner (proj (Status u v p d s h numX numY) dt i j) dt i (j + 1)
    | otherwise = Status u v p d s h numX numY

projectFields :: Status -> Float -> Int -> Status
projectFields (Status u v p d s h numX numY) dt iter
    | iter < numX - 1 = projectFields (projectInner (Status u v p d s h numX numY) dt iter 1) dt (iter + 1)
    | otherwise = Status u v p d s h numX numY

project :: Status -> Int -> Float -> Status
project (Status u v p d s h numX numY) iters dt = iterate (Status u v p d s h numX numY) 0
    where
        iterate stat ind --Gauss-Seidel relaxtion (Iteration iters times to approximate divergences(Since we use divergence to calculate divergence!))
            | ind <= iters = iterate (projectFields stat dt 1) (ind + 1)
            | otherwise = stat

{-
advectDensities :: Status -> Float -> Status
advectDensities (Status g b w h) dt = Status (mapPos (advectDen (Status g b w h) dt) g) b w h


advectDen :: Status -> Float -> (Int, Int) -> GridEntry -> GridEntry
advectDen (Status grid boxSize w h) dt (i, j) (GridEntry d (u1, v1) p s)
    | s == 1 = GridEntry new_dens (u1, v1) p s
    | otherwise = GridEntry d (u1, v1) p s
    where
        b2 = boxSize * 0.5
        u = (u1 + getX (getVel (getElem (i + 1) j grid))) * 0.5
        v = (v1 + getY (getVel (getElem i (j + 1) grid))) * 0.5
        (posx, posy) = (fromIntegral (i - 1) * boxSize + b2 - dt * u, fromIntegral (j - 1) * boxSize + b2 - dt * v)
        new_dens = sampleField (Status grid boxSize w h) posx posy Density
-}
{-
advectVelX :: Matrix GridEntry -> (Int, Int) -> (Float, Float) -> Float
advectVelX grid (floor_i, floor_j) (fract_i, fract_j) = lerp lerp1 lerp2 fract_j
    where
        lerp1 = lerp uij ui1j fract_i
        lerp2 = lerp uij1 ui1j1 fract_i
        
        uij = getX $ getVel $ getElem floor_i floor_j grid
        ui1j = getX $ getVel $ getElem (floor_i + 1) floor_j grid
        uij1 = getX $ getVel $ getElem floor_i (floor_j + 1) grid
        ui1j1 = getX $ getVel $ getElem (floor_i + 1) (floor_j + 1) grid


advectVelY :: Matrix GridEntry ->  (Int, Int) -> (Float, Float) -> Float
advectVelY grid (floor_i, floor_j) (fract_i, fract_j) = lerp lerp1 lerp2 fract_j
    where
        lerp1 = lerp vij vi1j fract_i
        lerp2 = lerp vij1 vi1j1 fract_i
        
        vij = getY $ getVel $ getElem floor_i floor_j grid
        vi1j = getY $ getVel $ getElem (floor_i + 1) floor_j grid
        vij1 = getY $ getVel $ getElem floor_i (floor_j + 1) grid
        vi1j1 = getY $ getVel $ getElem (floor_i + 1) (floor_j + 1) grid
-}
{-
--Copy of advectVel for densities
advectDen:: Matrix GridEntry -> Float -> Float -> Int -> Int -> (Int, Int) -> GridEntry -> GridEntry
advectDen grid boxSize dt w h (i, j) (GridEntry d v p s)
    | isNotBorder i j w h = GridEntry (advectD grid (fi, fj) (fri, frj)) v p s
    | otherwise = GridEntry d v p s
    where
        y_new = calculateV grid i j
        (posx, posy) = backtraceVelocity i j boxSize (GridEntry d (getX v, y_new) p s) dt
        (fi, fj) = (floor posx, floor posy) --floors
        (fri, frj) = (posx - fromIntegral fi, posy - fromIntegral fj) --fracts
-}
{-
advectD :: Matrix GridEntry ->  (Int, Int) -> (Float, Float) -> Float
advectD grid (floor_i, floor_j) (fract_i, fract_j) = lerp lerp1 lerp2 fract_j
    where
        lerp1 = lerp dij di1j fract_i
        lerp2 = lerp dij1 di1j1 fract_i
        
        dij = getDensity $ getElem floor_i floor_j grid
        di1j = getDensity $ getElem (floor_i + 1) floor_j grid
        dij1 = getDensity $ getElem floor_i (floor_j + 1) grid
        di1j1 = getDensity $ getElem (floor_i + 1) (floor_j + 1) grid

advectVel :: Matrix GridEntry -> Float -> Float -> Int -> Int -> (Int, Int) -> GridEntry -> GridEntry
advectVel grid boxSize dt w h (i, j) (GridEntry d v p s)
    | isNotBorder i j w h = GridEntry d (advectVelX grid (fi, fj) (fri, frj), advectVelY grid (fi, fj) (fri, frj)) p s
    | otherwise = GridEntry d v p s
    where
        y_new = calculateV grid i j
        (posx, posy) = backtraceVelocity i j boxSize (GridEntry d (getX v, y_new) p s) dt
        (fi, fj) = (floor posx, floor posy) --floors
        (fri, frj) = (posx - fromIntegral fi, posy - fromIntegral fj) --fracts
-}
--Projection
--Projection forces divergence to be 0 everywhere on the velocity field.
--This is the first equation of the Navier-Stokes equations, although it's not Navier or Stokes idea, it was Eulers.
--Euler's equations governed inviscid flow, Navier introduced viscosity into them.
--This is probably the most important part of this, but if you try to remove anything, everything will break, so idk ;)
{-
proj :: Matrix GridEntry -> Int -> Int -> Float -> Float -> Float -> Float -> Matrix GridEntry
proj g i j d s box dt = updateMat g 
                    [ --Divergence calculation
                    ((i, j), increPressure ( increVel 
                            (getElem i j g) 
                            ((d * fromIntegral (getS (getElem (i - 1) j g))) / s,
                             d * fromIntegral(getS (getElem i (j - 1) g)) / s))
                             ((d * getPressure(getElem i j g) * box) / (s * dt))), --Pressure increment
                    ((i + 1, j), increVel
                            (getElem i j g)
                            ((d * fromIntegral(getS (getElem (i + 1) j g))) / s, 0.0)),
                    ((i, j + 1), increVel
                            (getElem i j g)
                            (0.0, (d * fromIntegral(getS (getElem i (j + 1) g))) / s))
                    ]

projectOnce :: Status -> Int -> Int -> Float -> Status
projectOnce (Status grid b w h) i j dt
    | getS (getElem i j grid) == 1 = Status (proj grid i j d s b dt) b w h --Do not run for walls
    | otherwise = Status grid b w h
    where
        d = 1.9 * getX ( getVel(getElem (i + 1) j grid)) - getX (getVel(getElem i j grid)) + getY (getVel(getElem i (j + 1) grid)) - getY (getVel(getElem i j grid))
        s = fromIntegral ( getS(getElem (i + 1) j grid) + getS(getElem (i - 1) j grid) + getS(getElem i (j + 1) grid) + getS(getElem i (j - 1) grid) )
        
    
projectRow :: Status -> Int -> Int -> Float -> Status
projectRow stat i j dt
    | j == 1 || j == width = stat --Check for border columns
    | j < width - 1 = projectRow (projectOnce stat i j dt) i (j + 1) dt
    | j == width - 1 = projectOnce stat i j dt --Last case
    where
        width = getWidth stat

projectFull :: Status -> Int -> Float -> Status
projectFull stat i dt
    | i == 1 || i == height = stat --Check for border rows
    | i < height - 1 = projectFull (projectRow stat i 1 dt) (i + 1) dt
    | i == height - 1 = projectRow stat i 1 dt --Last case
    where
        height = getHeight stat

project :: Status -> Int -> Float -> Status 
project stat i dt
    | i <= 20 = projectFull stat 1 dt
    | otherwise = stat
-}
{-
extrapolate :: Status -> Status
extrapolate (Status g b w h) = Status (extrapY (extrapX g 0 w h) 0 w h) b w h
    where
        extrapX grid iter w h
            | iter <= w = [
                ((iter, 1), getIVX iter 2 grid),
                ((iter, h), getIVX iter (h - 1) grid)
            ] 
            | otherwise = []
        
        extrapY grid iter w h
            | iter <= h = [
                
            ]
-}