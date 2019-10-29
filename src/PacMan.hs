module PacMan where

import           Model
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic as Pt

-- Path Logic
upcomingNode :: PathNode -> PathNode -> Float -> (Int, Int) 
upcomingNode a b d = (round x, round y)
  where (x, y) = actualLocation [a, b] (fromIntegral $ floor d)

actualLocation [p] _     = toVector p
actualLocation (a:b:_) d = vb Pt.+ (d Pt.* n)
  where
    vb = toVector b
    delta = (toVector a) Pt.- vb
    n = normalizeV delta

toVector :: PathNode -> Vector
toVector (Pn x y) = (fromIntegral x, fromIntegral y)
