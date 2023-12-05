{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
--- Day 5: If You Give A Seed A Fertilizer ---

You take the boat and find the gardener right where you were told he would be:
managing a giant "garden" that looks more to you like a farm.

"A water source? Island Island is the water source!" You point out that Snow
Island isn't receiving any water.

"Oh, we had to stop the water because we ran out of sand to filter it with!
Can't make snow with dirty water. Don't worry, I'm sure we'll get more sand
soon; we only turned off the water a few days... weeks... oh no." His face sinks
into a look of horrified realization.

"I've been so busy making sure everyone here has food that I completely forgot
to check why we stopped getting more sand! There's a ferry leaving soon that
is headed over in that direction - it's much faster than your boat. Could you
please go check it out?"

You barely have time to agree to this request when he brings up another. "
While you wait for the ferry, maybe you can help us with our food production
problem. The latest Island Island Almanac just arrived and we're having
trouble making sense of it."

The almanac (your puzzle input) lists all of the seeds that need to be planted.
It also lists what type of soil to use with each kind of seed, what type of
fertilizer to use with each kind of soil, what type of water to use with each
kind of fertilizer, and so on. Every type of seed, soil, fertilizer and so on
is identified with a number, but numbers are reused by each category - that is,
soil 123 and fertilizer 123 aren't necessarily related to each other.

For example:

seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4

The almanac starts by listing which seeds need to be planted: seeds 79, 14, 55,
and 13.

The rest of the almanac contains a list of maps which describe how to convert
numbers from a source category into numbers in a destination category. That is,
the section that starts with seed-to-soil map: describes how to convert a seed
number (the source) to a soil number (the destination). This lets the gardener
and his team know which soil to use with which seeds, which water to use with
which fertilizer, and so on.

Rather than list every source number and its corresponding destination number
one by one, the maps describe entire ranges of numbers that can be converted.
  Each line within a map contains three numbers: the destination range start,
  the source range start, and the range length.

Consider again the example seed-to-soil map:

50 98 2
52 50 48

The first line has a destination range start of 50, a source range start of 98,
and a range length of 2. This line means that the source range starts at 98
and contains two values: 98 and 99. The destination range is the same length,
but it starts at 50, so its two values are 50 and 51. With this information,
you know that seed number 98 corresponds to soil number 50 and that seed
number 99 corresponds to soil number 51.

The second line means that the source range starts at 50 and contains 48 values:
50, 51, ..., 96, 97. This corresponds to a destination range starting at 52 and
also containing 48 values: 52, 53, ..., 98, 99. So, seed number 53 corresponds
to soil number 55.

Any source numbers that aren't mapped correspond to the same destination number.
So, seed number 10 corresponds to soil number 10.

So, the entire list of seed numbers and their corresponding soil numbers looks
like this:

seed  soil
0     0
1     1
...   ...
48    48
49    49
50    52
51    53
...   ...
96    98
97    99
98    50
99    51

With this map, you can look up the soil number required for each initial seed
number:

- Seed number 79 corresponds to soil number 81.
- Seed number 14 corresponds to soil number 14.
- Seed number 55 corresponds to soil number 57.
- Seed number 13 corresponds to soil number 13.

The gardener and his team want to get started as soon as possible, so they'd
like to know the closest location that needs a seed. Using these maps, find
the lowest location number that corresponds to any of the initial seeds. To do
this, you'll need to convert each seed number through other categories until
you can find its corresponding location number. In this example, the
corresponding types are:

- Seed 79, soil 81, fertilizer 81, water 81, light 74, temperature 78, humidity 78, location 82.
- Seed 14, soil 14, fertilizer 53, water 49, light 42, temperature 42, humidity 43, location 43.
- Seed 55, soil 57, fertilizer 57, water 53, light 46, temperature 82, humidity 82, location 86.
- Seed 13, soil 13, fertilizer 52, water 41, light 34, temperature 34, humidity 35, location 35.

So, the lowest location number in this example is 35.

What is the lowest location number that corresponds to any of the initial seed
numbers?
    -}
module Day.Five (solution1, solution2) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Range = Range {rBegin :: Int, rEnd :: Int}
  deriving (Ord, Eq)

instance Show Range where
  show r = "Range: {" <> show (rBegin r) <> ", " <> show (rEnd r) <> "}"

type Sources = Range

type Destinations = Range

type AlmanacMap = Map.Map Sources Destinations

data Almanac = Almanac
  { aSeeds :: [Int],
    aSeedToSoil :: AlmanacMap,
    aSoilToFertilizer :: AlmanacMap,
    aFertilizerToWater :: AlmanacMap,
    aWaterToLight :: AlmanacMap,
    aLightToTemperature :: AlmanacMap,
    aTemperatureToHumidity :: AlmanacMap,
    aHumidityToLocation :: AlmanacMap
  }
  deriving (Show)

emptyAlmanac :: Almanac
emptyAlmanac = Almanac mempty mempty mempty mempty mempty mempty mempty mempty

paragraphs :: T.Text -> [T.Text]
paragraphs = T.splitOn "\n\n"

parseSeeds :: T.Text -> Maybe [Int]
parseSeeds = fmap (fmap (read . T.unpack) . T.words) . T.stripPrefix "seeds: "

parseMap :: T.Text -> Maybe AlmanacMap
parseMap = fmap mconcat . traverse parseMapLine . T.lines

parseMapLine :: T.Text -> Maybe AlmanacMap
parseMapLine line =
  case read . T.unpack <$> T.words line of
    [d, s, l] -> Just $ Map.singleton (Range s (s + l)) (Range d (d + l))
    _ -> Nothing

parseAlmanac :: T.Text -> Maybe Almanac
parseAlmanac text = do
  [seeds, s2s, s2f, f2w, w2l, l2t, t2h, h2l] <- Just $ paragraphs text -- this automatically results in Nothing on filed pattern match
  aSeeds <- parseSeeds seeds
  aSeedToSoil <- parseMap =<< T.stripPrefix "seed-to-soil map:\n" s2s
  aSoilToFertilizer <- parseMap =<< T.stripPrefix "soil-to-fertilizer map:\n" s2f
  aFertilizerToWater <- parseMap =<< T.stripPrefix "fertilizer-to-water map:\n" f2w
  aWaterToLight <- parseMap =<< T.stripPrefix "water-to-light map:\n" w2l
  aLightToTemperature <- parseMap =<< T.stripPrefix "light-to-temperature map:\n" l2t
  aTemperatureToHumidity <- parseMap =<< T.stripPrefix "temperature-to-humidity map:\n" t2h
  aHumidityToLocation <- parseMap =<< T.stripPrefix "humidity-to-location map:\n" h2l
  pure Almanac {..}

inRange :: Int -> Range -> Bool
inRange i (Range b e) = b <= i && i <= e

lookupDestination :: Int -> Sources -> Destinations -> Maybe Int
lookupDestination i s d
  | i `inRange` s = Just (rBegin d + (i - rBegin s))
  | otherwise = Nothing

followMap :: Int -> AlmanacMap -> Int
followMap s m =
  fromMaybe s $
    Map.foldrWithKey (\k v acc -> acc <|> lookupDestination s k v) Nothing m

followAlmanac :: Almanac -> Int -> Int
followAlmanac Almanac {..} seed =
  let soil = followMap seed aSeedToSoil
      fertilizer = followMap soil aSoilToFertilizer
      water = followMap fertilizer aFertilizerToWater
      light = followMap water aWaterToLight
      temperature = followMap light aLightToTemperature
      humidity = followMap temperature aTemperatureToHumidity
      location = followMap humidity aHumidityToLocation
   in location

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let almanac = fromMaybe emptyAlmanac $ parseAlmanac contents
      seedLocations = followAlmanac almanac <$> aSeeds almanac
  print $ minimum seedLocations

{-
--- Part Two ---

Everyone will starve if you only plant such a small number of seeds. Re-reading
the almanac, it looks like the seeds: line actually describes ranges of seed
numbers.

The values on the initial seeds: line come in pairs. Within each pair, the first
value is the start of the range and the second value is the length of the range.
So, in the first line of the example above:

seeds: 79 14 55 13

This line describes two ranges of seed numbers to be planted in the garden.
The first range starts with seed number 79 and contains 14 values: 79, 80, ...,
91, 92. The second range starts with seed number 55 and contains 13 values: 55,
56, ..., 66, 67.

Now, rather than considering four seed numbers, you need to consider a total
of 27 seed numbers.

In the above example, the lowest location number can be obtained from seed
number 82, which corresponds to soil 84, fertilizer 84, water 84, light 77,
temperature 45, humidity 46, and location 46. So, the lowest location number is 46.

Consider all of the initial seed numbers listed in the ranges on the first
line of the almanac. What is the lowest location number that corresponds to
any of the initial seed numbers?
-}

reinterpreteSeedList :: [Int] -> [Range]
reinterpreteSeedList [] = []
reinterpreteSeedList [_] = []
reinterpreteSeedList (b : n : ss) = Range {rBegin = b, rEnd = b + n - 1} : reinterpreteSeedList ss

rangeLeftDiff :: Range -> Range -> [Range]
rangeLeftDiff l r
  | all (`inRange` r) [bL, eL] = []
  | all (< bL) [eR, bR] || all (> eL) [eR, bR] = pure l
  | (bR > bL) && (eR < eL) = [Range bL (bR - 1), Range (eR + 1) eL]
  | otherwise =
      pure $
        Range
          (if bL `inRange` r then eR + 1 else bL)
          (if eL `inRange` r then bR - 1 else eL)
  where
    bL = rBegin l
    eL = rEnd l
    bR = rBegin r
    eR = rEnd r

moveRange :: Int -> Range -> Range
moveRange i Range {rBegin, rEnd} = Range (rBegin + i) (rEnd + i)

rangeIntersection :: Range -> Range -> Maybe Range
rangeIntersection l r =
  if rEnd l >= rBegin r && rBegin l <= rEnd r
    then Just $ Range {rBegin = rBegin l `max` rBegin r, rEnd = rEnd l `min` rEnd r}
    else Nothing

diffWithList :: Range -> [Range] -> [Range]
diffWithList r = foldr f [r]
  where
    f a acc = mconcat . fromMaybe [] $ traverse (g a) acc
    g a x = let o = rangeLeftDiff x a in if null o then Nothing else pure o

mapRangeIntersectionFrom :: Range -> Sources -> Destinations -> Maybe Range
mapRangeIntersectionFrom r s d = do
  intersection <- rangeIntersection r s
  let distance = rBegin d - rBegin s
  pure $ moveRange distance intersection

rangeFollowMap :: AlmanacMap -> Range -> [Range]
rangeFollowMap m r =
  mconcat $ unmatched : matched
  where
    matched =
      Map.foldrWithKey (\s d acc -> maybeToList (mapRangeIntersectionFrom r s d) : acc) [] m
    unmatched = diffWithList r (Map.keys m)

followAlmanac2 :: Almanac -> [Range]
followAlmanac2 Almanac {..} = do
  range <- reinterpreteSeedList aSeeds
  soil <- rangeFollowMap aSeedToSoil range
  fertilizer <- rangeFollowMap aSoilToFertilizer soil
  water <- rangeFollowMap aFertilizerToWater fertilizer
  light <- rangeFollowMap aWaterToLight water
  temperature <- rangeFollowMap aLightToTemperature light
  humidity <- rangeFollowMap aTemperatureToHumidity temperature
  rangeFollowMap aHumidityToLocation humidity

minInRanges :: [Range] -> Int
minInRanges = minimum . fmap rBegin

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let almanac = fromMaybe emptyAlmanac $ parseAlmanac contents
      locationRanges = followAlmanac2 almanac
  print $ minInRanges locationRanges
