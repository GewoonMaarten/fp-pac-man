module Controllers.Animator
    ( updateAnimation
    )
where

import           Model
import           Models.Ghost
import           Models.PacMan

nextAnimStage :: (Bounded a, Enum a) => a -> a
nextAnimStage s | fromEnum s + 1 > fromEnum (getMaxBound s) = toEnum 0
                | otherwise = succ s
  where
    getMaxBound :: Bounded a => a -> a
    getMaxBound _ = maxBound

class Animatable a where
    updateAnimation :: Float -> a -> a

animationUpdateTime = 0.1

instance Animatable PacMan where
    updateAnimation secs p@(PacMan _ _ _ (Just (Movement dir stage)) _ it)
        | unAnimTimer p + secs > animationUpdateTime = p
            { unAnimTimer = 0
            , unMovement  = Just (Movement dir (nextAnimStage stage))
            }
        | otherwise = p { unAnimTimer = unAnimTimer p + secs }
    updateAnimation secs p = p { unAnimTimer = unAnimTimer p + secs }


ghostAnimationUpdateTime = 0.4
ghostAnimationUpdateTimeFast = 0.2
instance Animatable Ghost where
    updateAnimation secs g@(Ghost _ _ (Edible stage) _ _)
        | unGhostAnimTimer g + secs > ghostAnimationUpdateTime = g
            { unGhostAnimTimer = 0
            , unGhostState     = Edible (nextAnimStage stage)
            }
        | otherwise = g { unGhostAnimTimer = unGhostAnimTimer g + secs }
    updateAnimation secs g@(Ghost _ _ (Normal stage) _ _)
        | unGhostAnimTimer g + secs > ghostAnimationUpdateTime = g
            { unGhostAnimTimer = 0
            , unGhostState     = Normal (nextAnimStage stage)
            }
        | otherwise = g { unGhostAnimTimer = unGhostAnimTimer g + secs }
    updateAnimation secs g = g { unGhostAnimTimer = unGhostAnimTimer g + secs }
