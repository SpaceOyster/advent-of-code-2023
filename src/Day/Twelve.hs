{-# LANGUAGE OverloadedStrings #-}

module Day.Twelve (solution1, solution2) where

import Data.List (findIndices, group, intercalate, zip4)
import Data.String (IsString (..), fromString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Printf (IsChar (..))

data HotSpring = Operational | Damaged | Unknown
  deriving (Eq)

instance IsChar HotSpring where
  toChar s = case s of
    Operational -> '.'
    Damaged -> '#'
    Unknown -> '?'
  fromChar c = case c of
    '.' -> Operational
    '#' -> Damaged
    _ -> Unknown

instance Show HotSpring where
  show = show . toChar

instance {-# OVERLAPS #-} IsString [HotSpring] where
  fromString = fmap fromChar

instance {-# OVERLAPS #-} Show [HotSpring] where
  show = show . fmap toChar

type DmgdGroup = [HotSpring]

type UnknGroup = [HotSpring]

data ConditionRecord = CondRecord
  { crRow :: [HotSpring],
    crDamaged :: [Int]
  }
  deriving (Eq)

instance Show ConditionRecord where
  show cr = show (crRow cr) <> " " <> show (crDamaged cr)

parseCondRecord :: T.Text -> ConditionRecord
parseCondRecord text = case T.splitOn " " text of
  [r, ns] -> CondRecord {crRow = fromString $ T.unpack r, crDamaged = read . T.unpack <$> T.splitOn "," ns}
  _x -> undefined

parseRecords :: T.Text -> [ConditionRecord]
parseRecords = fmap parseCondRecord . T.lines

placeGroupInUnknown :: DmgdGroup -> UnknGroup -> [[HotSpring]]
placeGroupInUnknown dg ug
  | lUg < lDg = []
  | lUg == lDg = [dg]
  | otherwise = fmap (\n -> replicate n Operational <> dg <> replicate (nOperational - n) Operational) [0 .. nOperational]
  where
    lUg = length ug
    lDg = length dg
    nOperational = lUg - lDg

placeGroupsInUnknown :: [DmgdGroup] -> UnknGroup -> [[HotSpring]]
placeGroupsInUnknown [] ug = pure $ fmap (const Operational) ug
placeGroupsInUnknown (dg : dgs) ug = do
  placement <- placeGroupInUnknown dg ug
  let unoccupied = dropWhile (== Damaged) $ dropWhile (== Operational) placement
  let occupied = takeWhile (== Operational) placement <> dg <> take 1 unoccupied
  (occupied <>) <$> placeGroupsInUnknown dgs (Unknown <$ drop 1 unoccupied)

dumbSolution :: ConditionRecord -> [[HotSpring]]
dumbSolution cr =
  let dGroups = (`replicate` Damaged) <$> crDamaged cr
      allCombos = placeGroupsInUnknown dGroups (Unknown <$ crRow cr)
      f c = and $ zipWith (\crHS cHS -> crHS == Damaged && cHS == Damaged || crHS == Unknown || crHS == cHS) (crRow cr) c
   in filter f allCombos

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let condRecs = parseRecords contents
      arrNums = length . dumbSolution <$> condRecs
  print $ sum arrNums

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
