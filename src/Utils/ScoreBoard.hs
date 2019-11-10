module Utils.ScoreBoard
  ( Score
  , setScore
  , getScores
  , formatScores
  )
where

import           System.IO
import           Config
import           Paths_PacMan
import           Data.List.Split
import           Data.List
import           Data.Ord
import           Data.Function
import           Utils.Text

type Score = (Int, String)

setScore :: Score -> IO ()
setScore (scr, str) = do
  appendFile scoreBoardFilePath (str ++ ", " ++ show scr ++ "\n")

getScores :: IO [Score]
getScores = do
  contents <- readFile scoreBoardFilePath
  return (map (toScore . splitOn ", ") $ lines contents)
 where
  toScore :: [String] -> Score
  toScore [x, y] = (read y :: Int, x)

formatScores :: [Score] -> [Text]
formatScores = map printRow . take 3 . sortOn (Down . fst)
  where
    printRow (scr, str) = (str ++ gap str scr ++ show scr, Smallest)
    gap str scr = replicate (20 - (length str + length (show scr))) ' '