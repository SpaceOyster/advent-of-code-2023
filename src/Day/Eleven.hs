{-# LANGUAGE OverloadedStrings #-}

module Day.Eleven (solution1, solution2) where

import Data.List (elemIndices, findIndices, intercalate, nub, transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple (swap)

data Tile = Galaxy | EmptySpace
  deriving (Eq, Ord)

tileToChar :: Tile -> Char
tileToChar Galaxy = '@'
tileToChar EmptySpace = '.'

instance Show Tile where
  show = show . tileToChar

newtype GalaxyGrid = GalaxyGrid {ggGet :: [[Tile]]}
  deriving (Eq, Ord)

instance Show GalaxyGrid where
  show = intercalate "\n" . fmap (fmap tileToChar) . ggGet

data Coordinates = Coordinates {cX :: Int, cY :: Int}

instance Eq Coordinates where
  a == b = cX a == cX b && cY a == cY b

instance Show Coordinates where
  show c = show (cX c, cY c)

newtype GalaxyPair = GalaxyPair {gpPair :: (Coordinates, Coordinates)}
  deriving (Show)

instance Eq GalaxyPair where
  GalaxyPair a == GalaxyPair b = a == b || swap a == b

parseTile :: Char -> Tile
parseTile '#' = Galaxy
parseTile _ = EmptySpace

parseGalaxyGrid :: T.Text -> GalaxyGrid
parseGalaxyGrid = GalaxyGrid . fmap (fmap parseTile . T.unpack) . T.lines

expandGalaxy :: GalaxyGrid -> GalaxyGrid
expandGalaxy = GalaxyGrid . transpose . expandRows . transpose . expandRows . ggGet
  where
    expandRows g = g >>= \r -> if Galaxy `elem` r then [r] else [r, r]

findGalaxies :: GalaxyGrid -> [Coordinates]
findGalaxies =
  nub
    . concat
    . zipWith
      (\y r -> (`Coordinates` y) <$> elemIndices Galaxy r)
      [0 ..]
    . ggGet

uniquePairs :: [Coordinates] -> [GalaxyPair]
uniquePairs [] = []
uniquePairs (c : cs) = fmap (\x -> GalaxyPair (c, x)) cs <> uniquePairs cs

distanceInPair :: GalaxyPair -> Int
distanceInPair (GalaxyPair (a, b)) = abs (cY b - cY a) + abs (cX b - cX a)

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let galaxyGrid = expandGalaxy $ parseGalaxyGrid contents
      galaxies = findGalaxies galaxyGrid
      pairs = uniquePairs galaxies
  print $ sum $ distanceInPair <$> pairs

emptyRowsIndecies :: GalaxyGrid -> [Int]
emptyRowsIndecies = findIndices (notElem Galaxy) . ggGet

emptyColsIndecies :: GalaxyGrid -> [Int]
emptyColsIndecies = emptyRowsIndecies . GalaxyGrid . transpose . ggGet

expansionMultiplier :: Int
expansionMultiplier = 1000000

adjustGalaxiesCoords :: GalaxyGrid -> [Coordinates] -> [Coordinates]
adjustGalaxiesCoords gg = fmap f
  where
    expMult = expansionMultiplier - 1
    emptyRowsIx = emptyRowsIndecies gg
    emptyColsIx = emptyColsIndecies gg
    f c =
      let deltaX = (* expMult) $ length $ filter (< cX c) emptyColsIx
          deltaY = (* expMult) $ length $ filter (< cY c) emptyRowsIx
       in Coordinates {cX = cX c + deltaX, cY = cY c + deltaY}

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let galaxyGrid = parseGalaxyGrid contents
      galaxies = findGalaxies galaxyGrid
      adjustedGalaxies = adjustGalaxiesCoords galaxyGrid galaxies
      pairs = uniquePairs adjustedGalaxies
  print $ sum $ distanceInPair <$> pairs
