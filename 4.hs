#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Data.List (intersect)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

{- https://adventofcode.com/2023/day/4 -}

main = do
  cards <- readAndParseStdin []
  print $ part1 cards
  print $ part2 cards

-- sum up amount of winning numbers using the part1Score formula
part1 :: [Card] -> Int
part1 cards = sum $ map (score . length . getWinningNumbers) cards
  where
    score 0 = 0
    score n = 2 ^ (n - 1)

-- calculates number of cards won in part 2 of the task
part2 :: [Card] -> Int
part2 cards = sum $ calculateCardCopies $ map (length . getWinningNumbers) cards

-- starts with a base array of [1; n] and replicates Ns right for each `getWinningNumbers`
-- where N is the amount of card replicas
calculateCardCopies :: [Int] -> [Int]
calculateCardCopies xs = foldl replicateSingleCardWinnings (replicate (length xs) 1) (zip [0 ..] xs)

-- helper function for calling `copyCards` within `foldl`
replicateSingleCardWinnings :: [Int] -> (Int, Int) -> [Int]
replicateSingleCardWinnings cardReplicas (idx, winningNumbers) = copyCards cardReplicas (idx + 1) idx winningNumbers

-- copies N cards to `winningNumbers` elements right of `winningCardIdx` where N is `cardReplicas[winningCardIdx]`
copyCards :: [Int] -> Int -> Int -> Int -> [Int]
copyCards cardReplicas currIdx winningCardIdx winningNumbers
  | currIdx <= length cardReplicas && winningNumbers > 0 =
      let incrementedList = incrementAtIndex cardReplicas currIdx (cardReplicas !! winningCardIdx)
       in copyCards incrementedList (currIdx + 1) winningCardIdx (winningNumbers - 1)
  | otherwise = cardReplicas

-- takes a list, an index and an amount to increment by
incrementAtIndex :: [Int] -> Int -> Int -> [Int]
incrementAtIndex xs idx amount = take idx xs ++ [(xs !! idx) + amount] ++ drop (idx + 1) xs

-- gets the intersection of winning numbers and player numbers
getWinningNumbers :: Card -> [Int]
getWinningNumbers card = myNumbers card `intersect` winningNumbers card

data Card = Card
  { winningNumbers :: [Int],
    myNumbers :: [Int]
  }
  deriving (Show)

-- reads entirety of stdin and parses each line
readAndParseStdin :: [Card] -> IO [Card]
readAndParseStdin acc = do
  line <- getLine
  if null line
    then return acc
    else case parse cardParser "" line of
      Left parseError -> error $ show parseError
      Right card -> readAndParseStdin $ acc ++ [card]

-- parses a `Card [i]: [n1] [n2] [n3] | [n4] [n5] [n6]` line
cardParser :: Parser Card
cardParser = do
  _ <- string "Card" <* spaces <* many1 digit <* char ':' <* spaces
  winningNumbers <- numberParser
  _ <- char '|' <* spaces
  myNumbers <- numberParser

  return Card {winningNumbers, myNumbers}

-- reads a single number delimited by spaces
numberParser :: Parser [Int]
numberParser = map read <$> many1 digit `endBy` spaces
