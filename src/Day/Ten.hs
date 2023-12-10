{-# LANGUAGE OverloadedStrings #-}

module Day.Ten (solution1, solution2) where

import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Arr as Arr

newtype Pipe = Pipe {unPipe :: Char}
  deriving (Eq, Ord, Enum)

instance Show Pipe where
  show = show . unPipe

data Direction = North | West | South | East
  deriving (Show, Eq, Ord, Bounded, Enum)

type Point = (Int, Int)

type Start = Point

type Path = [Point]

type PipeMap = Arr.Array Point Pipe

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

parsePipeMap :: T.Text -> PipeMap
parsePipeMap text =
  let ls = T.unpack <$> T.lines text
      xbound = length $ head ls
      ybound = length ls
      indexedChars = zipWith (\l y -> zipWith (\c x -> ((x, y), Pipe c)) l [1 .. xbound]) ls [1 .. ybound]
   in Arr.array ((1, 1), (xbound, ybound)) $ mconcat indexedChars

findStart :: PipeMap -> Maybe Point
findStart pm =
  let inds = Arr.indices pm
   in find (\i -> unPipe (pm ! i) == 'S') inds

isPipe :: Char -> Bool
isPipe = (`elem` ("S|-LJ7F" :: [Char]))

inBounds :: Point -> PipeMap -> Bool
inBounds (x, y) pm =
  let ((minX, minY), (maxX, maxY)) = Arr.bounds pm
   in and [x >= minX, x < maxX, y >= minY, y < maxY]

stepDirection :: Point -> Direction -> Point
stepDirection (x, y) d =
  case d of
    North -> (x, y - 1)
    West -> (x - 1, y)
    South -> (x, y + 1)
    East -> (x + 1, y)

charAtPoint :: PipeMap -> Point -> Char
charAtPoint pm p = unPipe $ pm ! p

availableDirectionsFor :: Char -> [Direction]
availableDirectionsFor c =
  case c of
    'S' -> allDirections
    '|' -> [North, South]
    '-' -> [West, East]
    'L' -> [North, East]
    'J' -> [North, West]
    '7' -> [South, West]
    'F' -> [South, East]
    _ -> []

findAvailableSteps :: PipeMap -> Point -> [Point]
findAvailableSteps pm p
  | not (inBounds p pm) = []
  | otherwise = filter f $ stepDirection p <$> availableDirectionsFor (charAtPoint pm p)
  where
    f p' = inBounds p' pm && isPipe (charAtPoint pm p')

followLoop :: PipeMap -> Start -> Path
followLoop pm start = reverse $ go start [start]
  where
    go :: Point -> Path -> Path
    go p path = case filter (not . (`elem` path)) (findAvailableSteps pm p) of
      x : _ -> if x == start then start : path else go x (x : path)
      [] -> path

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let pm = parsePipeMap contents
      startM = findStart pm
      loopM = followLoop pm <$> startM
      farthestStepsM = fmap ((`div` 2) . length) loopM
  print startM
  print farthestStepsM

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
