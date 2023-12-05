{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day.Five (solution1, solution2) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  print "unimplemented"
