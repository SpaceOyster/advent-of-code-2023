{-# LANGUAGE OverloadedStrings #-}

module Day.Thirteen (solution1, solution2) where

import Control.Monad (guard)
import Data.List (intercalate, transpose)
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

transposePattern :: Pattern -> Pattern
transposePattern = Pattern . transpose . unPattern

findRepeatingRows :: Pattern -> [(Int, Int)]
findRepeatingRows (Pattern p) = do
  (idx, curr, next) <- zip3 [0 ..] p (drop 1 p)
  guard (curr == next)
  pure (idx, idx + 1)

isRowPairReflective :: Pattern -> (Int, Int) -> Bool
isRowPairReflective (Pattern p) (_a, b) =
  let (pa, pb) = splitAt b p
   in and $ zipWith (==) (reverse pa) pb

findReflectiveRows :: Pattern -> [(Int, Int)]
findReflectiveRows p = filter (isRowPairReflective p) $ findRepeatingRows p

findReflectiveCols :: Pattern -> [(Int, Int)]
findReflectiveCols = findReflectiveRows . transposePattern

parseRow :: T.Text -> TileRow
parseRow = fromString . T.unpack

parsePattern :: T.Text -> Pattern
parsePattern = Pattern . fmap parseRow . T.lines

parsePatterns :: T.Text -> [Pattern]
parsePatterns = fmap parsePattern . T.splitOn "\n\n"

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let patterns = parsePatterns contents
      reflectiveRows = findReflectiveRows <$> patterns
      reflectiveCols = findReflectiveCols <$> patterns
      answer = sum (fmap (sum . fmap snd) reflectiveCols) + 100 * sum (fmap (sum . fmap snd) reflectiveRows)
  print answer

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
