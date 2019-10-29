module Utils.Path where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic as Pt

data PathNode = Pn Int Int
  deriving(Eq)
type Path = [PathNode]
type NodeDistance = Float

-- Path Logic
upcomingNode :: [PathNode] -> Float -> (Int, Int) 
upcomingNode pns d = (round x, round y)
  where (x, y) = actualLocation pns (fromIntegral $ floor d)

actualLocation [p]     _ = toVector p
actualLocation (a:b:_) d = vb Pt.+ (d Pt.* n)
  where
    vb = toVector b
    delta = (toVector a) Pt.- vb
    n = if delta == (0,0) then (0,0) else normalizeV delta

toVector :: PathNode -> Vector
toVector (Pn x y) = (fromIntegral x, fromIntegral y)
