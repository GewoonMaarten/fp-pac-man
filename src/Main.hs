module Main where

import Level

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Pac-Man" (400, 500) (10, 10)) 
               black 
               30
               (pictures (showGridPure (buildGrid initGrid) (-180) (-230)))
