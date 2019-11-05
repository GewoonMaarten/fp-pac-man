module Utils.Path where

import           Graphics.Gloss hiding (Path)
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pt

type PathNode = (Int, Int)
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
toVector (x, y) = (fromIntegral x, fromIntegral y)

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

type CanPassFn = (Int, Int) -> Bool
type StepFn    = (Int, Int) -> (Int, Int)

-- implement ghost choice using GameState at this level
ghostFn :: CanPassFn -> Path -> Path -> Path
ghostFn canPass pmPath (P (a:b:_) _) = P p $ newDistance p
    where
        p = b : (follow canPass step b)
        step :: StepFn
        step (x, y) = (x + nx, y + ny)
        -- choose first direction we can move to as step
        (nx, ny) = head d
        d = directions canPass b a

newDistance :: [PathNode] -> Float
newDistance [_] = 0
newDistance (a:b:_)
    | outside a && outside b = 0
    | otherwise = fromIntegral $ l a b
    where
      -- when both spots are at maps boundaries it's a teleportation
      outside (x, y) = x <= 0 || y <= 0 || x >= 18 || y >= 20
      -- we can cheat vector length logic because we always move in a single direction
      l (xa, ya) (xb, yb) = abs $ (xb - xa) + (yb - ya)

dir :: PathNode -> PathNode -> (Int, Int)
dir (x1, y1) (x2, y2) = (d x1 x2, d y1 y2)
  where 
    d a b
      | a < b     = 1
      | a > b     = -1
      | otherwise = 0

-- First pass the current PathNode for which to resolve new directions.
-- Then pass the previous PathNode so we can exclude it to prevent turning around
directions :: CanPassFn -> PathNode -> PathNode -> [(Int, Int)]
directions canPass b@(x, y) a = filter canGoTo [(-1, 0), (1, 0), (0, -1), (0, 1)]
  where 
    canGoTo n@(nx, ny) = n /= (dir b a) && (canPass (x + nx, y + ny))

-- wrap boundary nodes to the other side
wrap :: (Int, Int) -> Maybe (Int, Int)
wrap (-1, y) = Just (18, y)
wrap (19, y) = Just (0, y)
wrap (x, -1) = Just (x, 20)
wrap (x, 21) = Just (x, 0)
wrap _ = Nothing

follow :: CanPassFn -> StepFn -> (Int, Int) -> [(Int, Int)]
follow canPass step p = f (wrap $ step p) (canPass $ step p) 
  where
    fl = follow canPass step
    -- wrap at bounds 
    f (Just wrapped) _    = p : wrapped : fl wrapped
    -- move forward
    f _              True = fl (step p)
    -- finish
    f _              _    = [p]
