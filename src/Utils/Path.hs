module Utils.Path where

import           Graphics.Gloss          hiding ( Path )
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic
                                               as Pt
import           Data.List
import           Data.Ord

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

locationImp [p        ] _ = toVector p
locationImp (a : b : _) d = vb Pt.+ (d Pt.* n)
 where
  vb    = toVector b
  delta = (toVector a) Pt.- vb
  n     = if delta == (0, 0) then (0, 0) else normalizeV delta

toVector :: PathNode -> Vector
toVector (x, y) = (fromIntegral x, fromIntegral y)

source p = head $ unNodes p
destination (P (_ : b : _) _) = b
destination p                 = source p

isStationary (P [_] _) = True
isStationary _         = False

{- MOVE -}

-- concrete default movePath function
movePath = movePath' nextFn

-- generic movePath function
-- nfn gets called when the next node is reached
-- this is used to implement custom behaviour at waypoints
movePath' _ _ p@(P [_] _) = p
movePath' nfn dt p | nd <= 0   = movePath' nfn (-nd) $ nfn p
                   | otherwise = p { unDistance = nd }
  where nd = unDistance p - dt

-- just move to the next node
nextFn (P (_ : npns) _) = P npns $ newDistance npns

type CanPassFn = PathNode -> Bool
type StepFn = PathNode -> PathNode

type PathDirection = (Int, Int)
type PickDirectionFn = PathNode -> [PathDirection] -> PathDirection

-- implement ghost choice using PacMan Path in PickDirectionFn
ghostFn :: CanPassFn -> PickDirectionFn -> Path -> Path
ghostFn canPass pick gp@(P (a : b : _) _) | outside b = nextFn gp
                                          | otherwise = P p $ newDistance p
 where
        -- create new path from node b following in direction d
  p  = b : follow canPass isFinished (stepFn d) b
  ds = directions canPass b a
  d  = pick b ds
  -- When we are not at the start node b and we are at a junction
  isFinished pn = pn /= b && atJunction canPass pn

-- Determine if we can choose directions again
atJunction canPass p = no && j
 where
  no = not (outside p)
  j  = (> 2) $ length $ directions canPass p p

newDistance :: [PathNode] -> Float
newDistance [_] = 0
newDistance (a : b : _) |
    -- when both spots are at maps boundaries it's a teleportation
                          outside a && outside b = 0
                        | otherwise              = fromIntegral $ l a b
  where
      -- we can cheat vector length logic because we always move in a single direction
        l (xa, ya) (xb, yb) = abs $ (xb - xa) + (yb - ya)

outside (x, y) = x <= 0 || y <= 0 || x >= 18 || y >= 20

dir :: PathNode -> PathNode -> PathDirection
dir (x1, y1) (x2, y2) = (d x1 x2, d y1 y2)
 where
  d a b | a < b     = 1
        | a > b     = -1
        | otherwise = 0

-- First pass the current PathNode for which to resolve new directions.
-- Then pass the previous PathNode so we can exclude it to prevent turning around
directions :: CanPassFn -> PathNode -> PathNode -> [PathDirection]
directions canPass b a = filter canGoTo [(-1, 0), (1, 0), (0, -1), (0, 1)]
 where
  canGoTo d = d /= revD && canPass (stepFn d b)
  revD = dir b a

bestDirection :: PathNode -> [PathDirection] -> PathNode -> PathDirection
bestDirection a ds b = head $ map snd $ sortOn Down $ map wv ds
 where
  n = normalizeV $ (toVector b) Pt.- (toVector a)
  wv :: PathDirection -> (Float, PathDirection)
  wv d = (dotV n (toVector d), d)

stepFn :: PathDirection -> StepFn
stepFn (nx, ny) (x, y) = (x + nx, y + ny)

-- wrap boundary nodes to the other side
wrap :: PathNode -> Maybe PathNode
wrap (-1, y ) = Just (18, y)
wrap (19, y ) = Just (0, y)
wrap (x , -1) = Just (x, 20)
wrap (x , 21) = Just (x, 0)
wrap _        = Nothing

-- IsFinished can be used to abort the path at another location
follow :: CanPassFn -> (PathNode -> Bool) -> StepFn -> PathNode -> [PathNode]
follow canPass isFinished step p = f (isFinished p)
                                     (wrap $ step p)
                                     (canPass $ step p)
 where
  fl = follow canPass isFinished step
  -- wrap at bounds 
  f False (Just wrapped) _    = p : wrapped : fl wrapped
  -- move forward
  f False _              True = fl (step p)
  -- finish
  f _     _              _    = [p]
