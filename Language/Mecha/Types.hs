{-# LANGUAGE  MultiParamTypeClasses #-}
module Language.Mecha.Types
  ( Vector, Vertex, Normal
  , Moveable  (..)
  , Scaleable (..)
  , Colorable (..)
  , Setable   (..)
  , Projectable (..)
  , moveX
  , moveY
  , moveZ
  , move3D
  , scaleAll
  , scaleX
  , scaleY
  , scaleZ
  , unions
  ) where
import Data.Colour
import qualified Data.Vector as V
type Vector = V.Vector Double
type Vertex = Vector
type Normal = Vector

class Moveable a where
  move    :: Vector -> a -> a
  rotateX :: Double -> a -> a
  rotateY :: Double -> a -> a
  rotateZ :: Double -> a -> a

move3D :: Moveable a => (Double,Double,Double) -> a -> a
move3D (x,y,z) = move $ V.fromList [x,y,z]

moveX :: Moveable a => Double -> a -> a
moveX a = move$ V.fromList [a,0,0] 

moveY :: Moveable a => Double -> a -> a
moveY a = move$ V.fromList [0,a,0] 


moveZ :: Moveable a => Double -> a -> a
moveZ a = move$ V.fromList [0,0,a] 

class Scaleable a where
  scale :: Vector -> a -> a

class Projectable a b  where
  projectZ :: Double -> a -> b
  section :: Bool -> b -> a
  rotateExtrude :: a -> b


scaleAll :: Scaleable a => Double -> a -> a
scaleAll a = scale $ V.fromList [a,a,a] 

scaleX :: Scaleable a => Double -> a -> a
scaleX a = scale $ V.fromList [a,0,0] 

scaleY :: Scaleable a => Double -> a -> a
scaleY a = scale $ V.fromList [0,a,0] 

scaleZ :: Scaleable a => Double -> a -> a
scaleZ a = scale $ V.fromList [0,0,a] 

class Colorable a where
  color :: AlphaColour Double -> a -> a

class Setable a where
  union        :: a -> a -> a
  intersection :: a -> a -> a
  difference   :: a -> a -> a

unions :: Setable a => [a] -> a
unions = foldl1 union

