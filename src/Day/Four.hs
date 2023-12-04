{-# LANGUAGE OverloadedStrings #-}

module Day.Four (solution1, solution2) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype CardScore = CardScore {getCardScore :: Int}
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

newtype CardNumber = CardNumber {getCardNumber :: Int}
  deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

data Card = Card
  { cNumber :: CardNumber,
    cWinningNumbers :: [Int],
    cNumbersGot :: [Int]
  }
  deriving (Show, Eq, Ord)

countWinningNumbersGot :: Card -> Int
countWinningNumbersGot c = sum (f <$> cNumbersGot c)
  where
    f x = if x `elem` cWinningNumbers c then 1 else 0

countCardScore :: Card -> CardScore
countCardScore c = if p <= 0 then 0 else 2 ^ (p - 1)
  where
    p = countWinningNumbersGot c

parseCards :: T.Text -> Maybe [Card]
parseCards = traverse parseCard . T.lines

parseCard :: T.Text -> Maybe Card
parseCard line =
  case T.splitOn ": " line of
    [cn, ns] -> go cn ns
    _ -> Nothing
  where
    go cn ns =
      case T.splitOn " | " ns of
        [winNs, nsGot] ->
          Card
            <$> parseCardNumber cn
            <*> Just (parseCardNumbers winNs)
            <*> Just (parseCardNumbers nsGot)
        _ -> Nothing

parseCardNumber :: T.Text -> Maybe CardNumber
parseCardNumber t = do
  x <- T.stripPrefix "Card " t
  pure $ CardNumber $ read $ T.unpack x

parseCardNumbers :: T.Text -> [Int]
parseCardNumbers = fmap (read . T.unpack) . T.words

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let cards = fromMaybe [] $ parseCards contents
      scores = countCardScore <$> cards
  print $ sum scores

generateWonCards :: [Card] -> Card -> [Card]
generateWonCards orig c =
  let n = countWinningNumbersGot c
   in take n $
        drop (getCardNumber $ cNumber c) orig

processCards :: [Card] -> [Card]
processCards orig = do
  mconcat $ go ((: []) <$> orig)
  where
    go [] = []
    go ([] : css) = go css
    go (cs : css) =
      let c = head cs
          l = length cs
          wCs = replicate l <$> generateWonCards orig c
       in cs : go (zipWith (<>) css (wCs <> repeat []))

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let cards = fromMaybe [] $ parseCards contents
      cardsWithWonCards = processCards cards
  print $ length cardsWithWonCards
