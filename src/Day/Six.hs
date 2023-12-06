{-# LANGUAGE OverloadedStrings #-}

module Day.Six (solution1, solution2) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

type Time = Int

type Distance = Int

type TimeF = Float

type DistanceF = Float

data RaceTableEntry = RaceTableEntry
  { raceTime :: Time,
    raceDistance :: Distance
  }
  deriving (Eq, Ord)

instance Show RaceTableEntry where
  show r = "RTE: {time: " <> show (raceTime r) <> ",dist:" <> show (raceDistance r) <> "}"

newtype RaceTable = RaceTable {raceTableEntries :: [RaceTableEntry]}
  deriving (Show, Eq, Ord)

parseRaceTable :: T.Text -> RaceTable
parseRaceTable text = case T.words <$> T.lines text of
  [ts, ds] -> RaceTable $ zipWith RaceTableEntry (f ts) (f ds)
  _ -> RaceTable []
  where
    f = fmap (read . T.unpack) . tail

-- difference betwwen actual time and expected time
calcDistanceDelta :: TimeF -> RaceTableEntry -> DistanceF
calcDistanceDelta t rte = t * (fromIntegral (raceTime rte) - t) - fromIntegral (raceDistance rte)

-- product function
calcDistanceDeltaProdF :: TimeF -> RaceTableEntry -> DistanceF
calcDistanceDeltaProdF t rte = fromIntegral (raceTime rte) - (2 * t)

-- find zero of product function touching main func on spot t
closestProdZero :: TimeF -> RaceTableEntry -> TimeF
closestProdZero t rte =
  let prod = calcDistanceDeltaProdF t rte
      curDist = calcDistanceDelta t rte
   in t - curDist / prod

sigma :: Float
sigma = 0.1

-- searching for zero with Newton method
searchForZero :: TimeF -> RaceTableEntry -> TimeF
searchForZero t rte
  | abs delta < sigma = t
  | otherwise = searchForZero (closestProdZero t rte) rte
  where
    delta = calcDistanceDelta t rte

data WinningTimeRange = WinningTimeRange {lowestWinningTime :: Time, highestWinningTime :: Time}
  deriving (Show, Eq, Ord)

findWinningTimeRange :: RaceTableEntry -> WinningTimeRange
findWinningTimeRange rte =
  let rteTarget = rte {raceDistance = raceDistance rte + 1}
      lowestWinningTime = ceiling $ searchForZero 0 rteTarget
      -- highestWinningTime = floor $ searchForZero (fromIntegral $ raceTime rte) rteTarget
      highestWinningTime = raceTime rte - lowestWinningTime -- this optimisation is possible because distance function is symetrical over center of [0, raceTime]
   in WinningTimeRange {lowestWinningTime, highestWinningTime}

countWinningTimes :: WinningTimeRange -> Int
countWinningTimes wtr = highestWinningTime wtr - lowestWinningTime wtr + 1

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let raceTable = parseRaceTable contents
      winningRanges = findWinningTimeRange <$> raceTableEntries raceTable
      countWTimes = countWinningTimes <$> winningRanges
  print countWTimes
  print $ product $ countWinningTimes <$> winningRanges

parseRaceTable2 :: T.Text -> RaceTable
parseRaceTable2 text = case T.words <$> T.lines text of
  [ts, ds] -> RaceTable $ pure $ RaceTableEntry (f ts) (f ds)
  _ -> RaceTable []
  where
    f = read . T.unpack . mconcat . tail

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let raceTable = parseRaceTable2 contents
      winningRanges = findWinningTimeRange <$> raceTableEntries raceTable
      countWTimes = countWinningTimes <$> winningRanges
  print winningRanges
  print countWTimes
