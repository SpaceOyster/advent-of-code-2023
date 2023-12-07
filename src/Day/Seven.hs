{-# LANGUAGE UndecidableInstances #-}

module Day.Seven (solution1, solution2) where

import Data.List (group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T

newtype Card = Card {getCard :: Char}
  deriving (Eq)

instance Show Card where
  show = show . getCard

instance Enum Card where
  fromEnum a = case getCard a of
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'T' -> 10
    'J' -> 11
    'Q' -> 12
    'K' -> 13
    'A' -> 14
    _ -> -1
  toEnum i = Card $ case i of
    10 -> 'T'
    11 -> 'J'
    12 -> 'Q'
    13 -> 'K'
    14 -> 'A'
    x -> head $ show x

instance Ord Card where
  compare a b = compare (fromEnum a) (fromEnum b)

newtype Hand c = Hand {hCards :: [c]}
  deriving (Show, Eq)

instance Ord (Hand Card) where
  compare (Hand a) (Hand b) =
    compare (findCombination a) (findCombination b) <> compare a b

data Combination
  = HighCard
  | OnePair
  | TwoPair
  | Three
  | FullHouse
  | Four
  | Five
  deriving (Show, Enum, Eq, Ord)

findCombination :: [Card] -> Combination
findCombination cs =
  let ls = fmap length . group . sort $ cs
   in f ls
  where
    f ls
      | 5 `elem` ls = Five
      | 4 `elem` ls = Four
      | 3 `elem` ls && 2 `elem` ls = FullHouse
      | 3 `elem` ls = Three
      | length (filter (== 2) ls) == 2 = TwoPair
      | length (filter (== 2) ls) == 1 = OnePair
      | otherwise = HighCard

type Bid = Int

data Competitor c = Competitor
  { cHand :: Hand c,
    cBid :: Bid
  }
  deriving (Show, Eq)

instance (Eq c, Ord (Hand c)) => Ord (Competitor c) where
  compare a b = compare (cHand a) (cHand b)

type Game = [Competitor Card]

totalWinnings :: (Eq c, Ord (Hand c)) => [Competitor c] -> Integer
totalWinnings game =
  let sortedGame = sort game
   in sum $ zipWith (\g n -> fromIntegral (cBid g * n)) sortedGame [1 ..]

parseGame :: T.Text -> Game
parseGame = fmap ((\[h, b] -> Competitor (Hand (Card <$> T.unpack h)) (read $ T.unpack b)) . T.words) . T.lines

solution1 :: FilePath -> IO ()
solution1 file = do
  contents <- T.readFile file
  let game = parseGame contents
  print $ totalWinnings game

newtype CardWJoker = CardWJoker {getJCard :: Char}
  deriving (Eq)

instance Show CardWJoker where
  show = show . getJCard

instance Enum CardWJoker where
  fromEnum a = case getJCard a of
    'J' -> 1
    x -> fromEnum $ Card x
  toEnum i = CardWJoker $ case i of
    1 -> 'J'
    x -> getCard $ toEnum x

instance Ord CardWJoker where
  compare a b = compare (fromEnum a) (fromEnum b)

instance Ord (Hand CardWJoker) where
  compare (Hand a) (Hand b) =
    compare (findCombinationJ a) (findCombinationJ b) <> compare a b

upgradeCombinationBy :: Combination -> Int -> Combination
upgradeCombinationBy c 0 = c
upgradeCombinationBy c 1 = case c of
  HighCard -> OnePair
  OnePair -> Three
  TwoPair -> FullHouse
  Three -> Four
  FullHouse -> FullHouse
  Four -> Five
  Five -> Five
upgradeCombinationBy c n = upgradeCombinationBy (upgradeCombinationBy c 1) (n - 1)

findCombinationJ :: [CardWJoker] -> Combination
findCombinationJ cs =
  let js = length (filter (== CardWJoker 'J') cs)
      withoutJs = (Card . getJCard <$> filter (/= CardWJoker 'J') cs)
      combWithoutJs = findCombination withoutJs
   in if js == 5 then Five else upgradeCombinationBy combWithoutJs js

type GameWithJokers = [Competitor CardWJoker]

parseGameJ :: T.Text -> GameWithJokers
parseGameJ = fmap ((\[h, b] -> Competitor (Hand (CardWJoker <$> T.unpack h)) (read $ T.unpack b)) . T.words) . T.lines

solution2 :: FilePath -> IO ()
solution2 file = do
  contents <- T.readFile file
  let game = parseGameJ contents
  print $ totalWinnings game
