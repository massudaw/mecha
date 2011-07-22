{-# LANGUAGE  MultiParamTypeClasses #-}
module Language.Mecha.Solid
  ( Solid     (..)
  , Plane     (..)
  , Primitive (..)
  , Primitive2D (..)
  , Transform (..)
  , Projection (..)
  , sphere
  , cone
  , box
  , cube
  , circle
  , cylinder
  , cylinder'
  , tube
  , radial
  , torus
  , newTorus
  ) where

import Language.Mecha.Types
import Data.Colour
import Data.Colour.Names (grey)

data Solid
  = Primitive [Transform] (AlphaColour Double ) Primitive
  | Extruded [Transform] (AlphaColour Double)  Projection  Plane
  | Union        Solid Solid
  | Intersection Solid Solid
  | Difference   Solid Solid
  deriving Eq

data Plane 
  = Primitive2D [Transform] (AlphaColour Double ) Primitive2D
  | Section [Transform] (AlphaColour Double ) Projection Solid
  | Union2D Plane Plane
  | Intersection2D Plane Plane
  | Difference2D  Plane Plane
    
  deriving Eq

data Primitive2D
  = Circle Double
  | Rectangle Double Double
  | Polygon [Vector] [[Vector]]
  deriving Eq

data Primitive
  = Sphere Double                -- ^ Diameter.
  | Cone   Double Double Double  -- ^ Bottom diameter, top diameter, height.
  | Box (Double, Double) (Double, Double) (Double, Double)  -- ^ (x min, x max) (y min, ymax) (z min, z max).
  | Torus  Double Double         -- ^ Major diameter, minor diameter.
  deriving Eq

data Transform
  = Scale (Vector)
  | Move  (Vector)
  | RotateX Double
  | RotateY Double
  | RotateZ Double
  deriving Eq

data Projection
  = Extrude Double
  | RotateExtrude
  | Cut Bool 
  deriving Eq


transform :: Transform -> Solid -> Solid 
transform t a = case a of
  Primitive    a b c   -> Primitive (a ++ [t]) b c
  Extruded     a b c d -> Extruded (a++[t]) b c d 
  Union        a b     -> Union         (transform t a) (transform t b)
  Intersection a b     -> Intersection  (transform t a) (transform t b)
  Difference   a b     -> Difference    (transform t a) (transform t b)

transform2D :: Transform -> Plane -> Plane
transform2D t a = case a of
  Primitive2D  a b c   -> Primitive2D (a ++ [t]) b c
  Union2D        a b     -> Union2D         (transform2D t a) (transform2D t b)
  Intersection2D a b     -> Intersection2D  (transform2D t a) (transform2D t b)
  Difference2D   a b     -> Difference2D    (transform2D t a) (transform2D t b)



projection :: Projection-> Plane -> Solid
projection p a = case a of
  Primitive2D    a b c   -> Extruded []  b  p  (Primitive2D a b c) 
  Section 	 a b c d -> Extruded [] b  p  (Section a b c d)
  Union2D        a b     -> Union         (projection p a) (projection p b)
  Intersection2D a b     -> Intersection  (projection p a) (projection p b)
  Difference2D   a b     -> Difference    (projection p a) (projection p b)

cut :: Projection -> Solid -> Plane 
cut p a = case a of
  Primitive    a b c   -> Section []  b p  (Primitive a b c) 
  Extruded     a b c d -> Section []  b p (Extruded a b c d) 
  Union        a b     -> Union2D        (cut p a) (cut p b)
  Intersection a b     -> Intersection2D  (cut p a) (cut p b)
  Difference   a b     -> Difference2D    (cut p a) (cut p b)



instance Projectable Plane Solid where
  projectZ a = projection $ Extrude a
  rotateExtrude = projection $ RotateExtrude 
  section a = cut $ Cut a

instance Moveable Solid where
  move a    = transform $ Move a
  rotateX a = transform $ RotateX a
  rotateY a = transform $ RotateY a
  rotateZ a = transform $ RotateZ a

instance Moveable Plane where
  move a    = transform2D $ Move a
  rotateX a = transform2D $ RotateX a
  rotateY a = transform2D $ RotateY a
  rotateZ a = transform2D $ RotateZ a

instance Scaleable Solid where
  scale a   = transform $ Scale a

instance Scaleable Plane where
  scale a   = transform2D $ Scale a


instance Setable Solid where
  union        = Union
  intersection = Intersection
  difference   = Difference

instance Setable Plane where
  union        = Union2D
  intersection = Intersection2D
  difference   = Difference2D


instance Colorable Solid where
  color c a = case a of
    Primitive    a _ b   -> Primitive a c b
    Extruded     a _ b d -> Extruded  a c b d
    Union        a b     -> Union         (color c a) (color c b)
    Intersection a b     -> Intersection  (color c a) (color c b)
    Difference   a b     -> Difference    (color c a) (color c b)

instance Colorable Plane where
  color c a = case a of
    Primitive2D    a _ b   -> Primitive2D a c  b
    Union2D        a b     -> Union2D         (color c a) (color c b)
    Intersection2D a b     -> Intersection2D  (color c a) (color c b)
    Difference2D   a b     -> Difference2D    (color c a) (color c b)


primitive :: Primitive -> Solid
primitive = Primitive [] (opaque grey)

primitive2D :: Primitive2D -> Plane 
primitive2D = Primitive2D [] (opaque grey)


-- | A sphere with diameter, centered at origin.
sphere :: Double -> Solid
sphere = primitive . Sphere

circle :: Double -> Plane 
circle = primitive2D . Circle

-- | A cube with edge length, centered at origin.
cube :: Double -> Solid
cube a = box c c c
  where
  b = a / 2
  c = (-b, b)

-- | A cone with base at the origin, given base diameter, top diameter, and height.
cone :: Double -> Double -> Double -> Solid
cone bd td h = primitive $ Cone bd td h

-- | A cylinder with base at the origin, given diameter and height.
cylinder :: Double -> Double -> Solid
cylinder d h = cone d d h

-- | Same as cylinder, but centered at the origin.
cylinder' :: Double -> Double -> Solid
cylinder' d h = moveZ (- h / 2) $ cylinder d h

-- | A hollow cylinder with base at the origin, given outer diameter, inner diamter, and height.
tube od id h = difference (cylinder od h) (moveZ (-h) $ cylinder id (4 * h))

-- | A box with ranges or X, Y, and Z positions.
box :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Solid
box x y z = primitive $ Box x y z

-- | Arranges a solid in a radial pattern.
radial :: (Double -> Solid) -> Int -> Solid
radial f n = unions [ rotateZ a $ f a | i <- [0 .. n - 1], let a = 2 * pi * fromIntegral i / fromIntegral n ]

-- | A torus centered at the origin, aligned on the z-axis, with the major and minor diameters.
torus :: Double -> Double -> Solid
torus d1 d2 = primitive $ Torus d1 d2

newTorus :: Double -> Double -> Solid
newTorus d1 d2 = rotateExtrude $ move3D (d1,0,0) $ circle d2


