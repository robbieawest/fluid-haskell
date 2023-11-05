module Main where

import Prelude hiding (replicate, head, tail)
import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Data.Vector hiding(map)
import Data.Matrix


main :: IO ()
main = simulate
            (InWindow "Euler Fluid Solver"
                        (900, 900)
                        (10, 10))
            black
            144
            (initialStatus 20 20)
            draw
            step




updateMat :: Matrix a -> [((Int, Int), a)] -> Matrix a
updateMat m [] = m
updateMat m (((i, j), v):vs) = updateMat (setElem v (i, j) m) vs

{-
updateMat m vc
 | vc == empty = m
 | otherwise = update (m ! i) (fromList (j, val)) `cons` updateMat m vcs
    where
        ((i, j), val) = head vc
        vcs = tail vc
-}
data GridEntry = GridEntry {
    density :: Float,
    velocity :: Vec2f, -- Point :: (Float, Float)
    pressure :: Float,
    s :: Int --Decides whether the entry is a wall. 1 for not wall, 0 for wall
} deriving(Eq)

instance Show GridEntry where
    show (GridEntry d v p s) =
        show d Prelude.++ "," Prelude.++ show v Prelude.++ "," Prelude.++ show p Prelude.++ "," Prelude.++ show s



type Vec2f = Point

getVel :: GridEntry -> Vec2f
getVel (GridEntry _ v _ _) = v

setVel :: GridEntry -> Vec2f -> GridEntry
setVel (GridEntry a _ b c) v = GridEntry a v b c

addVel :: Vec2f -> Vec2f -> Vec2f
addVel (x,y) (x1,y1) = (x + x1, y + y1)

getX :: Vec2f -> Float
getX (x, _) = x

setX :: Vec2f -> Float -> Point
setX (_, a) x = (x, a)

setY :: Vec2f -> Float -> Point
setY (a, _) y = (a, y)

getY :: Vec2f -> Float
getY (_, y) = y

getDensity :: GridEntry -> Float
getDensity (GridEntry u _ _ _) = u

getPressure :: GridEntry -> Float
getPressure (GridEntry _ _ p _) = p

getS :: GridEntry -> Int
getS (GridEntry _ _ _ s) = s

increVel :: GridEntry -> Vec2f -> GridEntry
increVel (GridEntry d v p s) nv = GridEntry d (v `addVel` nv) p s

increPressure :: GridEntry -> Float -> GridEntry 
increPressure (GridEntry d v p s) f = GridEntry d v (p + f) s

data Status = Status {
    grid :: Matrix GridEntry, --This may seem wierd as this is an Euler fluid solver, but the particles do not really move
    boxSize :: Float,
    width :: Int, --Where 1 is 1 box grid
    height :: Int
} deriving(Show)

getWidth :: Status -> Int 
getWidth (Status _ _ w _) = w

getHeight :: Status -> Int
getHeight (Status _ _ _ h) = h


addGravity :: Float -> (Int, Int) -> GridEntry -> GridEntry
addGravity dt _ (GridEntry d (x, y) p s) = GridEntry d (x, y - 9.8 * dt) p s

modifyVelocities :: Float -> Status -> Status
modifyVelocities dt (Status g b w h) = Status (mapPos (addGravity dt) g) b w h

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
        d = getX ( getVel(getElem (i + 1) j grid)) - getX (getVel(getElem i j grid)) + getY (getVel(getElem i (j + 1) grid)) - getY (getVel(getElem i j grid))
        s = fromIntegral ( getS(getElem (i + 1) j grid) + getS(getElem (i - 1) j grid) + getS(getElem i (j + 1) grid) + getS(getElem i (j - 1) grid) )
        
                            


projectRow :: Status -> Int -> Int -> Float -> Status
projectRow stat i j dt
    | j == 1 || j == width = stat --Check for border columns
    | j < width - 1 = projectRow (projectOnce stat i j dt) i (j + 1) dt
    | j == width - 1 = projectOnce stat i j dt --Last case
    where
        width = getWidth stat

project :: Status -> Int -> Float -> Status
project stat i dt
    | i == 1 || i == height = stat --Check for border rows
    | i < height - 1 = project (projectRow stat i 1 dt) (i + 1) dt
    | i == height - 1 = projectRow stat i 1 dt --Last case
    where
        height = getHeight stat


lerp :: Float -> Float -> Float -> Float
--Linear interpolation function
lerp a b k = a + k * (b - a)

getStaggeredPosition :: Float -> Int -> Int -> Vec2f
--This calculates the staggered position of the grid at zero indexes i, j
--Namely the position of the _u::Float component of the _v::Vec2f velocity, as opposed to the _v::Float component
--Where height is the box size (Square so width = height)
getStaggeredPosition height i j = (height * fromIntegral i,
                          height * fromIntegral j + height / 2.0)


backtraceVelocity :: Int -> Int -> GridEntry -> Float -> Vec2f
--This calculates the position after backtracing the velocity of a certain grid entry
--Note that this position does not necessarily fall on the staggered grid, 
--therefore you have to interpolate(simply a weighted sum) between the surrounding velocities to get the new velocity in advection.
--delta time for stability
backtraceVelocity i j ge dt = (fromIntegral i - dt * u, fromIntegral j - dt * v)
    where
        v_x = getVel ge
        u = getX v_x
        v = getY v_x

calculateV :: Matrix GridEntry -> Int -> Int -> Float
--This calculates the y component of the velocity by averaging the Y components around it
--Used in the advection process before backtracing
calculateV grid i j = (getY (getVel (getElem i j grid)) +
                       getY (getVel (getElem i (j + 1) grid)) +
                       getY (getVel (getElem (i - 1) j grid)) +
                       getY (getVel (getElem (i - 1) (j + 1) grid))
                       ) / 4.0

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

advectD :: Matrix GridEntry ->  (Int, Int) -> (Float, Float) -> Float
advectD grid (floor_i, floor_j) (fract_i, fract_j) = lerp lerp1 lerp2 fract_j
    where
        lerp1 = lerp dij di1j fract_i
        lerp2 = lerp dij1 di1j1 fract_i
        
        dij = getDensity $ getElem floor_i floor_j grid
        di1j = getDensity $ getElem (floor_i + 1) floor_j grid
        dij1 = getDensity $ getElem floor_i (floor_j + 1) grid
        di1j1 = getDensity $ getElem (floor_i + 1) (floor_j + 1) grid

advectVel :: Matrix GridEntry -> Float -> Float -> (Int, Int) -> GridEntry -> GridEntry
advectVel grid boxSize dt (i, j) (GridEntry d v p s) = GridEntry d (advectVelX grid (fi, fj) (fri, frj), advectVelY grid (fi, fj) (fri, frj)) p s
    where
        (GridEntry d (x_new, y_new) p s) = GridEntry d (getX v, calculateV grid i j) p s
        (posx, posy) = backtraceVelocity i j (GridEntry d (x_new, y_new) p s) dt
        (fi, fj) = (floor posx, floor posy) --floors
        (fri, frj) = (posx - fromIntegral fi, posy - fromIntegral fj) --fracts

--Copy of advectVel for densities
advectDen:: Matrix GridEntry -> Float -> Float -> (Int, Int) -> GridEntry -> GridEntry
advectDen grid boxSize dt (i, j) (GridEntry d v p s) = GridEntry (advectD grid (fi, fj) (fri, frj)) v p s
    where
        (GridEntry d (x_new, y_new) p s) = GridEntry d (getX v, calculateV grid i j) p s
        (posx, posy) = backtraceVelocity i j (GridEntry d (x_new, y_new) p s) dt
        (fi, fj) = (floor posx, floor posy) --floors
        (fri, frj) = (posx - fromIntegral fi, posy - fromIntegral fj) --fracts

advectVelocities :: Status -> Float -> Status
advectVelocities (Status g b w h) dt = Status (mapPos (advectVel g b dt) g) b w h

advectDensities :: Status -> Float -> Status
advectDensities (Status g b w h) dt = Status (mapPos (advectDen g b dt) g) b w h




calculatePressureColour :: Float -> Float -> Float -> Color
calculatePressureColour pressure minP maxP = case indicator of
        0 -> makeColor 0.0 val 1.0 1.0
        1 -> makeColor 0.0 1.0 1.0 1.0
        2 -> makeColor val 1.0 0.0 1.0
        3 -> makeColor 1.0 (1.0 - val) 0.0 1.0
    where
        pressureVal = min (max pressure minP) (maxP - 0.001)
        pressureDifference = maxP - minP
        pressureVal2
            | pressureDifference == 0 = 0.5
            | otherwise = (pressureVal - minP) / pressureDifference
        amplify = 0.25
        indicator = fromIntegral (floor (pressureVal2 / amplify))
        val = (pressureVal2 - (indicator * amplify)) / amplify;


drawGridEntry :: Float -> Float -> Float -> (Int, Int) -> GridEntry -> Picture
drawGridEntry b minP maxP (i, j) (GridEntry d v p s) = trans $ col rect --translation is to set at a certain position, very functional
    where
        col = color $ calculatePressureColour p minP maxP
        trans = translate ((fromIntegral j - 0.5) * b - 450.0) ((fromIntegral i - 0.5) * b - 450.0)-- j -> x , i -> y
        rect = rectangleSolid b b

minMaxPressure :: [GridEntry] -> Float -> (Float -> Float -> Bool) -> Float
minMaxPressure [] curr minMax = curr
minMaxPressure (x:xs) curr minMax
    | p `minMax` curr = minMaxPressure xs p minMax
    | otherwise = minMaxPressure xs curr minMax
    where p = getPressure x


draw :: Status -> Picture
draw (Status g b w h) = Pictures ( Data.Matrix.toList (mapPos (drawGridEntry b minP maxP) g ))
    where
        listG = Data.Matrix.toList g
        minP = minMaxPressure listG 0.0 (<)
        maxP = minMaxPressure listG 0.0 (>)


{-
checkRow :: Vector GridEntry -> Int -> Int -> Int -> Int -> Vector GridEntry
checkRow empty w h i j = empty --Empty as defined in Data.Vector representing empty vector
checkRow (x `cons` xs) w h i j
    | not (isBorder i j w h) = x : checkRow xs w h i (j + 1)
    | otherwise  = GridEntry d v p 0 : checkRow xs w h i (j + 1)
    where x = GridEntry d v p s

addBorder :: Matrix GridEntry -> Int -> Int -> Int -> Matrix GridEntry
addBorder empty w h i = empty
-- addBorder (singleton (empty)) w h i = singleton empty
addBorder (row:rows) w h i = checkRow row w h i 0 : addBorder rows w h (i + 1)
-}

isNotBorder :: Int -> Int -> Int -> Int -> Bool
isNotBorder i j w h = i > 1 && i < h && j > 1 && j < w

addBorder :: Status -> Status
addBorder (Status g b w h) = Status 
                                (mapPos (
                                    \(i, j) (GridEntry d v p s) ->
                                         if isNotBorder i j w h
                                            then GridEntry d v p 1
                                            else GridEntry d v p 0)
                                        g)
                                b w h    

initMatrix  :: Int -> Int -> GridEntry -> Matrix GridEntry
--initMatrix w h def = matrix w h (\(i, j) -> def)
initMatrix w h def = matrix w h (\(i, j) -> GridEntry 0.0 (0.0, 0.0) ((fromIntegral i * fromIntegral j) / 20.0) 1)

initialStatus :: Int -> Int -> Status
initialStatus width height = addBorder $ Status 
                    (initMatrix width height (GridEntry 0.0 (0.0, 0.0) 0.0 1))
                    (900.0 / fromIntegral width)
                    width
                    height

step :: ViewPort -> Float -> Status -> Status
step view dt status = advectDensities (advectVelocities (
                        project (modifyVelocities dt status) 1 dt) dt) dt
--step view dt status = advectDensities (advectVelocities(modifyVelocities dt status) dt) dt
--step view dt status = status
