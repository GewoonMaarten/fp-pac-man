module Utils.Path where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt

data PathNode = Pn Int Int
  deriving(Eq, Show)
type NodeDistance = Float
data Path = P {
  unNodes :: [PathNode],
  unDistance :: NodeDistance
} deriving (Show)

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

{- MOVE -}

-- concrete default movePath function
movePath = movePath' nextFn

-- generic movePath function
-- nfn gets called when the next node is reached
-- this is used to implement custom behaviour at waypoints
movePath' _   _  p@(P [_] _) = p
movePath' nfn dt p
  | nd <= 0   = movePath' nfn (-nd) $ nfn p
  | otherwise = p { unDistance = nd }
  where 
    nd = (unDistance p) - dt

-- just move to the next node
nextFn (P (_:npns) _) = P npns $ newDistance npns

-- implement ghost choice using GameState at this level
ghostFn gs p@(P (a:b:_) _) = P [b, a] $ newDistance [b, a]

newDistance [_] = 0
-- we can cheat vector length logic because we always move in a single direction
newDistance ((Pn xa ya):(Pn xb yb):_) = fromIntegral $ abs $ (xb - xa) + (yb - ya)
