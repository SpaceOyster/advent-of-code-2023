{-# LANGUAGE OverloadedStrings #-}

module Day.Fourteen (solution1, solution2) where

import Data.List (elemIndices, intercalate, sort, transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Arr as Arr
import Text.Printf (IsChar (..))

data Tile = O | C | G
  deriving (Eq)

instance IsChar Tile where
  fromChar c = case c of
    '#' -> C
    'O' -> O
    _ -> G
  toChar t = case t of
    C -> '#'
    O -> 'O'
    G -> '.'

instance Show Tile where
  show = show . toChar

instance Ord Tile where
  C <= _ = True
  O <= G = True
  O <= _ = False
  G <= _ = False

type Point = (Int, Int)

newtype Field = Field {fRows :: Arr.Array Point Tile}
  deriving (Eq)

instance Show Field where
  show (Field f) =
    let ((_, _), (_, ymax)) = Arr.bounds f
        colLength = ymax
        toCols :: [Tile] -> [[Tile]]
        toCols [] = []
        toCols x = take colLength x : toCols (drop colLength x)
        rows = transpose $ toCols $ Arr.elems f
     in intercalate "\n" $ fmap (fmap toChar) rows

parseField :: T.Text -> Field
parseField text =
  let ls = T.unpack <$> T.lines text
      xbound = length $ head ls
      ybound = length ls
      indexedChars = zipWith (\l y -> zipWith (\c x -> ((x, y), fromChar c)) l [1 .. xbound]) ls [1 .. ybound]
   in Field $ Arr.array ((1, 1), (xbound, ybound)) $ mconcat indexedChars

tiltColumn :: [Tile] -> [Tile]
tiltColumn ts = go ts cubes
  where
    cubes = elemIndices C ts
    go ts' [] = sort ts'
    go [] _cs = error "bad :("
    go ts' (c : cs) = sort (take c ts') <> take 1 (drop c ts') <> go (drop (c + 1) ts') ((\x -> x - (c + 1)) <$> cs)

tiltNorth :: Field -> Field
tiltNorth (Field arr) =
  let ((xmin, ymin), (xmax, ymax)) = Arr.bounds arr
      colNums = Arr.range (xmin, xmax)
      rowNums = Arr.range (ymin, ymax)
      col x = fmap (\y -> arr ! (x, y)) rowNums
      arrUpdate = concatMap f colNums
      f x = zip (map (x,) rowNums) (tiltColumn $ col x)
   in Field $ arr // arrUpdate

northEdgeLoad :: Field -> Int
northEdgeLoad (Field arr) =
  let ((_, _), (_, ymax)) = Arr.bounds arr
      allPoints = Arr.range $ Arr.bounds arr
      f (x, y) acc = if arr ! (x, y) == O then acc + (ymax - y + 1) else acc
   in foldr f 0 allPoints

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let field = parseField contents
      answer = northEdgeLoad $ tiltNorth field
  print answer

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
