module Render where

import Data.Vector as V
import Prelude hiding (replicate, head, tail)

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort

import Base
import Simulation

--Colour functions

calculatePressureColour :: Float -> Float -> Float -> Color
calculatePressureColour pressure minP maxP = case indicator of
        0 -> makeColor 0.0 val 1.0 1.0
        1 -> makeColor 0.0 1.0 (1.0 - val) 1.0
        2 -> makeColor val 1.0 0.0 1.0
        3 -> makeColor 1.0 (1.0 - val) 0.0 1.0
    where
        pressureVal = min (max pressure minP) (maxP - 0.0001)
        pressureDifference = maxP - minP

        pressureVal2
            | pressureDifference == 0 = 0.5
            | otherwise = (pressureVal - minP) / pressureDifference
        amplify = 0.25

        indicator = fromIntegral (floor (pressureVal2 / amplify))
        val = (pressureVal2 - (indicator * amplify)) / amplify;

calculateDensityColour :: Float -> Color
calculateDensityColour dens = makeColor denst denst denst 1.0
    where denst = min dens 1.0

{-
drawGridEntry :: Bool -> Float -> Float -> Float -> (Int, Int) -> GridEntry -> Picture
drawGridEntry pressureView b minP maxP (i, j) (GridEntry d v p s) = trans $ col rect --translation is to set at a certain position, very functional
    where
        col = if pressureView then color (calculatePressureColour p minP maxP) else color (calculateDensityColour d)
        trans = translate ((fromIntegral j - 0.5) * b - 450.0) ((fromIntegral i - 0.5) * b - 450.0)-- j -> x , i -> y
        rect = rectangleSolid b b
-}

drawGridParticle :: Bool -> Status -> Float -> Float -> Int -> Float -> Picture
drawGridParticle pressureView (Status u v p d s h numX numY) minP maxP ind sCurr = trans $ col rect
    where
        (i, j) = i1i2 ind numY
        col = if pressureView then color (calculatePressureColour (indf i j numY p) minP maxP) else color (calculateDensityColour (indf i j numY d))
        trans = translate ((fromIntegral j + 0.5) * h - 450.0) ((fromIntegral i + 0.5) * h - 450.0)-- j -> x , i -> y
        rect = rectangleSolid h h

--Main draw function
draw :: Bool -> Status -> Picture
draw pressureView (Status u v p d s h numX numY) = Pictures (toList(
                                                        imap (drawGridParticle pressureView 
                                                             (Status u v p d s h numX numY) minP maxP) s
                                                   ))
    where
        minP = V.minimum p
        maxP = V.maximum p