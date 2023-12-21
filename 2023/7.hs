#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (readAndParseStdin)
import Data.List
import Data.Ord
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

{- https://adventofcode.com/2023/day/7 -}

main :: IO ()
main = do
  games <- readAndParseStdin parseAllGames
  print $ part1 games
  print $ part2 games

-- sums the score of each game
part1 :: [Game] -> Int
part1 games = calcScore $ sortBy gameComparator games
  where
    gameComparator = comparing (Down . getHandStrength . groupCards . cards) <> comparing cards

-- maps jacks to jokers (lowest scoring card, which can transmorph itself to the highest scoring group)
-- and sums the score of each game
part2 :: [Game] -> Int
part2 games = calcScore $ sortBy gameComparator $ mapAllJacksToJokers games
  where
    gameComparator = comparing (Down . getHandStrength . transmorphJokers . cards) <> comparing cards

-- transforms every jack in a game to a joker
mapAllJacksToJokers :: [Game] -> [Game]
mapAllJacksToJokers = map (\game -> game {cards = map mapJackToJoker $ cards game})
  where
    mapJackToJoker Jack = Joker
    mapJackToJoker a = a

-- transmorphs jokers into whatever the highest scoring rank was
transmorphJokers :: [Rank] -> [Int]
transmorphJokers [Joker, Joker, Joker, Joker, Joker] = groupCards [Joker, Joker, Joker, Joker, Joker]
transmorphJokers cards = (head grouped + jokerCount) : tail grouped
  where
    cardsWithoutJokers = filter (/= Joker) cards
    jokerCount = length cards - length cardsWithoutJokers
    grouped = groupCards cardsWithoutJokers

-- calculates the final score of the game
calcScore :: [Game] -> Int
calcScore game = sum $ zipWith (curry formula) [1 ..] game
  where
    formula (idx, game) = baseScore game * idx

-- determines the strength of the hand based on cards in hand
getHandStrength :: [Int] -> HandStrength
getHandStrength sortedCardCount = case sortedCardCount of
  [5] -> FiveOfAKind
  [4, 1] -> FourOfAKind
  [3, 2] -> FullHouse
  (3 : _) -> ThreeOfAKind
  (2 : 2 : _) -> TwoPair
  (2 : _) -> OnePair
  _ -> HighCard

-- groups any ranks together, returning the amount of items per group. ie. [J, J, A, A, A, 1] would return [3, 2, 1]
groupCards :: [Rank] -> [Int]
groupCards = sortOn Down . map length . group . sort

-- parses each game delimited by a newline
parseAllGames :: Parser [Game]
parseAllGames = parseGame `endBy` char '\n'

-- parses games in the format `[C][C][C] 123`
parseGame :: Parser Game
parseGame = do
  cards <- many1 parseRank <* spaces
  baseScore <- read <$> many1 digit
  return Game {cards, baseScore}

-- parses a single card rank
parseRank :: Parser Rank
parseRank = do
  c <- oneOf "23456789TJQKA"
  return $ case c of
    '2' -> Two
    '3' -> Three
    '4' -> Four
    '5' -> Five
    '6' -> Six
    '7' -> Seven
    '8' -> Eight
    '9' -> Nine
    'T' -> Ten
    'J' -> Jack
    'Q' -> Queen
    'K' -> King
    'A' -> Ace

data Game = Game
  { cards :: [Rank],
    baseScore :: Int
  }
  deriving (Show)

data Rank = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)

data HandStrength = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoOfAKind | TwoPair | OnePair | HighCard deriving (Eq, Ord, Enum, Show)
