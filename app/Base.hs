
--This file contains the basic data structs and functions
module Base where
import Data.Vector as V
import Prelude hiding (replicate, head, tail, map)

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort

--Vec2f type (makes more sense)
type Vec2f = Point


isNotBorder :: Int -> Int -> Int -> Int -> Bool
isNotBorder i j w h = i > 0 && i < h - 1 && j > 0 && j < w - 1

isNotBorderOne :: Int -> Int -> Int -> Bool --Takes single 1D index, shapens to 2D then calcualtes
isNotBorderOne ind w h = isNotBorder i j w h
    where
        i = ind `div` h
        j = ind `rem` h --Same as `mod` but mod takes negative values

addBorder :: Status -> Status
addBorder (Status u v p d s b numX numY) = Status u v p d (imap borderI s) b numX numY
    where
        borderI i val 
            | isNotBorderOne i numX numY = val
            | otherwise = 0.0


-- Statuses

data Status = Status {
    u :: !(Vector Float),
    v :: !(Vector Float),
    p :: !(Vector Float),
    d :: !(Vector Float),
    s :: !(Vector Float),
    h :: !Float,
    numX :: !Int, --Where 1 is 1 box grid
    numY :: !Int
} deriving(Show)

indf :: Int -> Int -> Int -> Vector Float -> Float
indf i j numY field = field V.! (i * numY + j)

i2i1 :: (Int, Int) -> Int -> Int
i2i1 (i, j) h = i * h + j

i1i2 :: Int -> Int -> (Int, Int)
i1i2 ind h = (i, j)
    where
        i = ind `div` h
        j = ind `rem` h


initialStatus :: Int -> Int -> Status
initialStatus numX numY = addBorder $ Status 
                    (V.replicate (numX * numY) 0.0) --u
                    (V.replicate (numX * numY) 0.0) --v
                    (V.replicate (numX * numY) 0.0) --p
                    (V.replicate (numX * numY) 0.0) --d
                    (V.replicate (numX * numY) 1.0) --s
                    (900.0 / fromIntegral numX)
                    numX
                    numY

