module Interact where

import Data.Strict.Vector as V
import Prelude hiding (replicate, head, tail, map)

import Graphics.Gloss hiding (Vector)
import Graphics.Gloss.Data.ViewPort
import Base


--Functions for setup e.g. adding velocities and gravity

--Add square in the middle with some upward velocity
addVelocity :: Status -> Status
addVelocity (Status u v p d s h numX numY) = Status u (v // vUpdate) p d s h numX numY
    where
        vUpdate = [
            (i2i1 (numX `div` 2, numY `div` 2) numY, 3.0),
            (i2i1 ((numX `div` 2) + 1, numY `div` 2) numY, 3.0),
            (i2i1 (numX `div` 2, (numY `div` 2) + 1) numY, 3.0),
            (i2i1 ((numX `div` 2) + 1, (numY `div` 2) + 1) numY, 3.0)
            ]
        