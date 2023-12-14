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
     in ("\n" <>) $ (<> "\n\n") $ intercalate "\n" $ fmap (fmap toChar) rows

parseField :: T.Text -> Field
parseField text =
  let ls = T.unpack <$> T.lines text
      xbound = length $ head ls
      ybound = length ls
      indexedChars = zipWith (\l y -> zipWith (\c x -> ((x, y), fromChar c)) l [1 .. xbound]) ls [1 .. ybound]
   in Field $ Arr.array ((1, 1), (xbound, ybound)) $ mconcat indexedChars

data Direction = North | West | South | East
  deriving (Show, Eq, Ord, Bounded, Enum)

tiltColumn :: [Tile] -> [Tile]
tiltColumn ts = go ts cubes
  where
    cubes = elemIndices C ts
    go ts' [] = sort ts'
    go [] _cs = error "bad :("
    go ts' (c : cs) = sort (take c ts') <> take 1 (drop c ts') <> go (drop (c + 1) ts') ((\x -> x - (c + 1)) <$> cs)

tiltNorth :: Field -> Field
tiltNorth f = scanRowsFromDirection f North tiltColumn

scanRowsFromDirection :: Field -> Direction -> ([Tile] -> [Tile]) -> Field
scanRowsFromDirection (Field arr) d fun =
  let ((xmin, ymin), (xmax, ymax)) = Arr.bounds arr
      colNums = (if d == East then reverse else id) $ Arr.range (xmin, xmax)
      rowNums = (if d == South then reverse else id) $ Arr.range (ymin, ymax)
      col x = fmap (\y -> arr ! (x, y)) rowNums
      row y = fmap (\x -> arr ! (x, y)) colNums
      f x = zip (map (x,) rowNums) (fun $ col x)
      g y = zip (map (,y) colNums) (fun $ row y)
      arrUpdate =
        if d `elem` [North, South]
          then concatMap f colNums
          else concatMap g rowNums
   in Field $ arr // arrUpdate

runTiltSequence :: Field -> [Direction] -> [Field]
runTiltSequence = scanl (\f' d -> scanRowsFromDirection f' d tiltColumn)

runTiltCycle :: Field -> Cycle
runTiltCycle f =
  let [cNorth, cWest, cSouth, cEast] = drop 1 $ runTiltSequence f [North, West, South, East]
   in Cycle {cNorth, cWest, cSouth, cEast}

nextCycle :: Cycle -> Cycle
nextCycle = runTiltCycle . cEast

-- It just felt like it would cycle, it seems like in general it wouldn't cycle
-- from the start, though. so my solution is make many (200) cycles and see if
-- the 200 cycle already appeared before, now the index difference would be the
-- period value. It would be much faster to somehow calculate period in one go.
findCyclePeriod :: Field -> Int
findCyclePeriod f =
  let firstCycle = runTiltCycle f
      el200 = iterate nextCycle firstCycle !! 200
      x = last $ tail $ elemIndices el200 $ take 200 $ iterate nextCycle firstCycle
   in 200 - x

fbillion :: Field -> Field
fbillion f =
  let firstCycle = runTiltCycle f
      period = findCyclePeriod f
      foreferSeq = iterate nextCycle firstCycle
      idx = 200 - 1 + ((1000000000 - 200) `mod` period)
   in cEast $ foreferSeq !! idx

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

data Cycle = Cycle
  { cNorth :: Field,
    cWest :: Field,
    cSouth :: Field,
    cEast :: Field
  }
  deriving (Show, Eq)

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let field = parseField contents
      fbill = fbillion field
  print $ northEdgeLoad fbill
