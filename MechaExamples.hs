module Main (main) where

import Language.Mecha
import Data.Colour.Names
import Data.Colour

wheelThick = 4 
wheelDiameter = 55
smallWheelDiameter = 10
smallWheelThick= 2
numberSmallWheel= 36 
ringDiameter = 55
ringThick = 8
numberScrews = 4
screwType= 2 
screwLength=10
screwM4HeadLength= 1.5 
shaftDiameter= 6
shaftLength= 50 

shaft=color (green `withOpacity` 0.9 )$ move3D (0,0,-1)$  cylinder shaftDiameter shaftLength 

cyl1=cylinder wheelDiameter wheelThick

rolete = box  (-smallWheelThick/2,smallWheelThick/2) (wheelDiameter/2-smallWheelDiameter/2,wheelDiameter/2+1) (-1,wheelThick+1)

screwHole = move3D (10,0,0) $ screwM4Tail

roletes = unions $ map (\x-> rotateZ (x*2*pi/numberSmallWheel) rolete) [1.0,2..numberSmallWheel] ++  map (\x-> rotateZ (x*2*pi/numberScrews) screwHole) [1.0,2..numberScrews] 
 
ring = color (opaque blue)$ torus 50 1.5 

screwM4 = unions [ screwM4Head, screwM4Tail]

hexagono radius length sides= color(opaque black) $ move3D (0,0,-0.1)$ difference (cylinder (radius*2) length ) (unions $ map (\x-> rotateZ (x*2*pi/sides ) (boxDimensions)) [1.0,2..sides] )

	where boxDimensions = box (((cos (2*pi/sides))*  radius),radius+1) (-1*radius,radius) (0, length+0.1)

 
screwM4Head  = move3D (0,0,-1*screwM4HeadLength) $difference  (color (red `withOpacity` 0.8)$ cylinder screwType screwM4HeadLength )( hexagono 1 0.5 6 ) 

screwM4Tail = color (opaque red)$ cylinder (screwType/2) screwLength

screwsM4= unions$  map (\x-> rotateZ (x*2*pi/numberScrews)$move3D (10,0,0) screwM4) [1.0,2..numberScrews] 

cilindro = move3D (0,0,30)$projectZ 10 $difference ( circle 10) $color (green `withOpacity` 0.5) (circle 5)
wheel = difference cyl1 (unions [roletes,ring,shaft] ) 

secao =  section True $ shaft ::Plane
cil = projectZ 10 secao


montagem= unions [ move3D (0,0,-20) $move3D (0,0,-1*shaftLength) shaft 
		 , move3D (0,0,-5) ring 
		 , move3D (0,0,0) wheel 
		 , move3D (0,0,-25) screwsM4
		 , cilindro
		 , move3D (0,0,-10)$ rotateX pi wheel]
		 


main :: IO ()
main = do
  writeFile "csg.scad" $ openSCAD  $cil 
  putStrLn ""
  putStrLn "Writing file: csg.scad"
  putStrLn ""
  putStrLn "Open with OpenSCAD, then click Design->Compile."
  putStrLn ""
