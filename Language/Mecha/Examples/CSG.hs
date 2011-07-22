-- | A constructive solid geometry widget.
module Language.Mecha.Examples.CSG (csg) where

import Language.Mecha
import Data.Colour
import qualified Data.Vector as V
-- | A CSG widget.
csg :: Solid
csg = unions
  [ move (V.fromList [ 0, 0, 0]) $ difference sphereCube cyl3
  , move (V.fromList [ 0, 4, 0]) $ cyl3
  ]

sphere' = sphere 2
cube' = cube $ 2 * 0.75
sphereCube = intersection sphere' cube'
cyl = moveZ (-1) $ cylinder 1 2
cyl3 = unions [cyl, rotateX (pi / 2) cyl, rotateY (pi / 2) cyl]

