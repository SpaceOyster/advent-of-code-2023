{-# LANGUAGE OverloadedStrings #-}

module Day.Ten (solution1, solution2) where

import Data.List (find)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Arr as Arr

data Tile
  = S
  | NS
  | WE
  | NE
  | NW
  | SW
  | SE
  | G
  deriving (Eq, Ord, Enum)

instance Show Tile where
  show p =
    show $ case p of
      S -> 'S'
      NS -> '|'
      WE -> '-'
      NE -> 'L'
      NW -> 'J'
      SW -> '7'
      SE -> 'F'
      G -> '.'

data Direction = North | West | South | East
  deriving (Show, Eq, Ord, Bounded, Enum)

type Point = (Int, Int)

type Start = Point

type Path = [Point]

type PipeMap = Arr.Array Point Tile

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

parseTile :: Char -> Tile
parseTile c = case c of
  'S' -> S
  '|' -> NS
  '-' -> WE
  'L' -> NE
  'J' -> NW
  '7' -> SW
  'F' -> SE
  _ -> G

parsePipeMap :: T.Text -> PipeMap
parsePipeMap text =
  let ls = T.unpack <$> T.lines text
      xbound = length $ head ls
      ybound = length ls
      indexedChars = zipWith (\l y -> zipWith (\c x -> ((x, y), parseTile c)) l [1 .. xbound]) ls [1 .. ybound]
   in Arr.array ((1, 1), (xbound, ybound)) $ mconcat indexedChars

findStart :: PipeMap -> Maybe Point
findStart pm =
  let inds = Arr.indices pm
   in find (\i -> (pm ! i) == S) inds

isPipe :: Tile -> Bool
isPipe G = False
isPipe _ = True

inBounds :: Point -> PipeMap -> Bool
inBounds p pm = Arr.inRange (Arr.bounds pm) p

stepDirection :: Point -> Direction -> Point
stepDirection (x, y) d =
  case d of
    North -> (x, y - 1)
    West -> (x - 1, y)
    South -> (x, y + 1)
    East -> (x + 1, y)

availableDirectionsFor :: Tile -> [Direction]
availableDirectionsFor c =
  case c of
    S -> allDirections
    NS -> [North, South]
    WE -> [West, East]
    NE -> [North, East]
    NW -> [North, West]
    SW -> [South, West]
    SE -> [South, East]
    _ -> []

findAvailableSteps :: PipeMap -> Point -> [Point]
findAvailableSteps pm p
  | not (inBounds p pm) = []
  | otherwise = filter f $ stepDirection p <$> availableDirectionsFor (pm ! p)
  where
    f p' = inBounds p' pm && isPipe (pm ! p')

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
  let pm = parsePipeMap test1
  print "unimplemented"

test1 :: T.Text
test1 =
  T.intercalate
    "\n"
    [ "7-F7-",
      ".FJ|7",
      "SJLL7",
      "|F--J",
      "LJ.LJ"
    ]

test2 :: T.Text
test2 =
  T.intercalate
    "\n"
    [ "...........",
      ".S-------7.",
      ".|F-----7|.",
      ".||.....||.",
      ".||.....||.",
      ".|L-7.F-J|.",
      ".|..|.|..|.",
      ".L--J.L--J.",
      "..........."
    ]

test3 :: T.Text
test3 =
  T.intercalate
    "\n"
    [ ".F----7F7F7F7F-7....",
      ".|F--7||||||||FJ....",
      ".||.FJ||||||||L7....",
      "FJL7L7LJLJ||LJ.L-7..",
      "L--J.L7...LJS7F-7L7.",
      "....F-J..F7FJ|L7L7L7",
      "....L7.F7||L7|.L7L7|",
      ".....|FJLJ|FJ|F7|.LJ",
      "....FJL-7.||.||||...",
      "....L---J.LJ.LJLJ..."
    ]

test4 :: T.Text
test4 =
  T.intercalate
    "\n"
    [ "FF7FSF7F7F7F7F7F---7",
      "L|LJ||||||||||||F--J",
      "FL-7LJLJ||||||LJL-77",
      "F--JF--7||LJLJ7F7FJ-",
      "L---JF-JLJ.||-FJLJJ7",
      "|F|F-JF---7F7-L7L|7|",
      "|FFJF7L7F-JF7|JL---7",
      "7-L-JL7||F7|L7F-7F7|",
      "L.L7LFJ|||||FJL7||LJ",
      "L7JLJL-JLJLJL--JLJ.L"
    ]
