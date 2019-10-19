module Main where

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Pac-Man" (200, 200) (0, 0)) black (Circle 80)
