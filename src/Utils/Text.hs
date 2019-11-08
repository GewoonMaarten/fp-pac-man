module Utils.Text where

import           Config
import           Graphics.Gloss

type Text = (String, TextSize)
data TextSize = Large | Medium | Small | Smallest deriving (Show)

textScale :: TextSize -> Picture -> Picture
textScale Large    = scale 0.5 0.5
textScale Medium   = scale 0.35 0.35
textScale Small    = scale 0.2 0.2
textScale Smallest = scale 0.15 0.15

txtToPic :: Text -> Picture
txtToPic (str, scale) = textScale scale $ color white $ text str

txtsToPic :: Float -> Float -> [Text] -> Picture
txtsToPic initX initY = pictures . foldl (flip f) []
 where
  f txt [] = [translate initX initY (txtToPic txt)]
  f txt@(str, Large) acc@(Translate x y _ : _) =
    translate x (y - 70) (txtToPic txt) : acc
  f txt@(str, Medium) acc@(Translate x y _ : _) =
    translate x (y - 50) (txtToPic txt) : acc
  f txt@(str, Small) acc@(Translate x y _ : _) =
    translate x (y - 35) (txtToPic txt) : acc
  f txt@(str, Smallest) acc@(Translate x y _ : _) =
    translate x (y - 20) (txtToPic txt) : acc

floatLeft :: Float -> Float
floatLeft = (+) (-(fromIntegral screenWidth / 2))
