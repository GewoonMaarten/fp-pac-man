module Utils.Path where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt

data PathNode = Pn Int Int
  deriving(Eq)
type NodeDistance = Float
data Path = P {
  unNodes :: [PathNode],
  unDistance :: NodeDistance
}

upcomingNode :: Path -> (Int, Int) 
upcomingNode (P pns d) = (round x, round y)
  where (x, y) = locationImp pns (fromIntegral $ floor d)

actualLocation (P pns d) = locationImp pns d

locationImp [p]     _ = toVector p
locationImp (a:b:_) d = vb Pt.+ (d Pt.* n)
  where
    vb = toVector b
    delta = (toVector a) Pt.- vb
    n = if delta == (0,0) then (0,0) else normalizeV delta

toVector :: PathNode -> Vector
toVector (Pn x y) = (fromIntegral x, fromIntegral y)

movePath _  p@(P [_] _) = p
movePath dt p
  | nd <= 0   = movePath (-nd) $ next p
  | otherwise = p { unDistance = nd }
  where 
    nd = (unDistance p) - dt
    next (P (_:npns) _) = P npns $ dif npns
    dif [_] = 0
    dif ((Pn xa ya):(Pn xb yb):_) = fromIntegral $ abs $ (xb - xa) + (yb - ya)
