{-# LANGUAGE OverloadedStrings #-}

module Day.Ten (solution1, solution2) where

import Data.List (elemIndex, findIndex, groupBy, nub, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Pipe = Pipe {tPipe :: Char}
  deriving (Show, Eq, Ord, Enum)

data Direction = North | West | South | East
  deriving (Show, Eq, Ord, Bounded, Enum)

type Point = (Int, Int)

type Bounds = (Point, Point)

type Start = Point

type Path = [Point]

type Field = [String]

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

toField :: T.Text -> Field
toField = fmap T.unpack . T.lines

findStart :: Field -> Maybe (Int, Int)
findStart field = do
  y <- findIndex (elem 'S') field
  x <- elemIndex 'S' (field !! y)
  return (x, y)

inBounds :: Point -> Bounds -> Bool
inBounds (x, y) ((minX, minY), (maxX, maxY)) =
  and [x >= minX, x < maxX, y >= minY, y < maxY]

stepDirection :: Point -> Direction -> Point
stepDirection (x, y) d =
  case d of
    North -> (x, y - 1)
    West -> (x - 1, y)
    South -> (x, y + 1)
    East -> (x + 1, y)

charAtPoint :: Field -> Point -> Char
charAtPoint field (x, y) = field !! y !! x

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

findAvailableSteps :: Field -> Bounds -> Point -> [Point]
findAvailableSteps field b p
  | not (inBounds p b) = []
  | otherwise = filter f $ stepDirection p <$> availableDirectionsFor (charAtPoint field p)
  where
    f p' = inBounds p' b && (charAtPoint field p' `elem` ("S|-LJ7F" :: [Char]))

followLoop :: Field -> Start -> Path
followLoop field start = reverse $ go start [start]
  where
    bounds = ((0, 0), (length $ head field, length field))
    go :: Point -> Path -> Path
    go p path = case filter (not . (`elem` path)) (findAvailableSteps field bounds p) of
      x : _ -> if x == start then path else go x (x : path)
      [] -> path

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let field = toField contents
      startM = findStart field
      loopM = followLoop field <$> startM
      farthestStepsM = fmap ((`div` 2) . length) loopM
  print farthestStepsM

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
