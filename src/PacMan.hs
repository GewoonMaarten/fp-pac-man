module PacMan where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Data.Point.Arithmetic as Pt

data PathNode = Pn Int Int
  deriving(Eq)
type Path = [PathNode]
type NodeDistance = Float

data PacMan = PacMan {
    unPath :: Path,
    unDistance :: NodeDistance
}

initialPacMan :: PacMan
-- initialPacMan = PacMan [Pn 0 0, Pn 100 0, Pn 100 100, Pn 0 100, Pn 0 0] 1
initialPacMan = PacMan [Pn 9 7] 1

showPacMan (PacMan pns d) = translate x (-y) $ color yellow $ circleSolid 10
  where (x, y) = ((-180, -255) Pt.+ 20 Pt.* (actualLocation pns d))

-- Path Logic
upcomingNode :: [PathNode] -> Float -> (Int, Int) 
upcomingNode pns d = (round x, round y)
  where (x, y) = actualLocation pns (fromIntegral $ floor d)

actualLocation [p] _     = toVector p
actualLocation (a:b:_) d = vb Pt.+ (d Pt.* n)
  where
    vb = toVector b
    delta = (toVector a) Pt.- vb
    n = if delta == (0,0) then (0,0) else normalizeV delta

toVector :: PathNode -> Vector
toVector (Pn x y) = (fromIntegral x, fromIntegral y)
