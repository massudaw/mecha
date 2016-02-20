module Main (main) where

import System.Process

import Language.Mecha
import Language.Mecha.Examples.CSG
import qualified Data.Text.IO as T

main :: IO ()
main = do
  putStrLn "Writing file csg.scad.  Opening with OpenSCAD ..."
  T.writeFile "csg.scad" $ openSCAD $ scaleAll 10 $ csg
  readProcess "OpenSCAD" ["csg.scad"] ""
  return ()

