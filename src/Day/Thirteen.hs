{-# LANGUAGE OverloadedStrings #-}

module Day.Thirteen (solution1, solution2) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (elemIndices, intercalate, transpose)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String (IsString (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf (IsChar (..))

data Tile = D | G
  deriving (Eq)

instance IsChar Tile where
  fromChar c = case c of
    '#' -> D
    _ -> G
  toChar t = case t of
    D -> '#'
    G -> '.'

instance Show Tile where
  show = show . toChar

type TileRow = [Tile]

instance {-# OVERLAPS #-} IsString TileRow where
  fromString = fmap fromChar

instance {-# OVERLAPS #-} Show TileRow where
  show = show . fmap toChar

newtype Pattern = Pattern {unPattern :: [TileRow]}
  deriving (Eq)

instance Show Pattern where
  show p = "\nPattern: [\n" <> (intercalate "\n" . fmap show $ unPattern p) <> "\n]\n"

type RowIdx = Int

parseRow :: T.Text -> TileRow
parseRow = fromString . T.unpack

parsePattern :: T.Text -> Pattern
parsePattern = Pattern . fmap parseRow . T.lines

parsePatterns :: T.Text -> [Pattern]
parsePatterns = fmap parsePattern . T.splitOn "\n\n"

transposePattern :: Pattern -> Pattern
transposePattern = Pattern . transpose . unPattern

findRepeatingRows :: Pattern -> [(RowIdx, RowIdx)]
findRepeatingRows (Pattern p) = do
  (idx, curr, next) <- zip3 [0 ..] p (drop 1 p)
  guard (curr == next)
  pure (idx, idx + 1)

isRowPairReflective :: Pattern -> (RowIdx, RowIdx) -> Bool
isRowPairReflective (Pattern p) (_a, b) =
  let (pa, pb) = splitAt b p
   in and $ zipWith (==) (reverse pa) pb

findReflectiveRows :: Pattern -> [(RowIdx, RowIdx)]
findReflectiveRows p = filter (isRowPairReflective p) $ findRepeatingRows p

findReflectiveCols :: Pattern -> [(RowIdx, RowIdx)]
findReflectiveCols = findReflectiveRows . transposePattern

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let patterns = parsePatterns contents
      reflectiveRows = findReflectiveRows <$> patterns
      reflectiveCols = findReflectiveCols <$> patterns
      answer = sum (fmap (sum . fmap snd) reflectiveCols) + 100 * sum (fmap (sum . fmap snd) reflectiveRows)
  print answer

countDifferentTiles :: TileRow -> TileRow -> Int
countDifferentTiles ra rb =
  let eqs = zipWith (==) ra rb
      idxs = elemIndices False eqs
   in length idxs

findReflRowWSmudge :: Pattern -> Maybe (RowIdx, RowIdx)
findReflRowWSmudge (Pattern p) = listToMaybe $ do
  idx <- [0 .. length p]
  let (pa, pb) = splitAt (idx + 1) p
      difference = sum $ zipWith countDifferentTiles (reverse pa) pb
  guard (difference == 1)
  pure (idx, idx + 1)

findReflColWSmudge :: Pattern -> Maybe (RowIdx, RowIdx)
findReflColWSmudge = findReflRowWSmudge . transposePattern

sumMirrors :: Pattern -> Int
sumMirrors p = fromMaybe 0 (fmap ((* 100) . snd) (findReflRowWSmudge p) <|> fmap snd (findReflColWSmudge p))

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let patterns = parsePatterns contents
      mirrorsNumbers = sumMirrors <$> patterns
      answer = sum mirrorsNumbers
  print mirrorsNumbers
  print answer
