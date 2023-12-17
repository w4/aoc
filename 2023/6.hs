#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Control.Applicative ((<*))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Aoc (readAndParseStdin)

{- https://adventofcode.com/2023/day/6 -}

main = do
  game <- readAndParseStdin gameParser
  print $ part1 game
  print $ part2 game

-- returns the product of how many winning times there are per game
part1 :: [(Int, Int)] -> Int
part1 = product . map (length . getWinningTimes)
  where
    getWinningTimes (time, winCond) = filter (> winCond) $ map (`distance` time) [1 .. time - 1]

-- concatenates all games into one big game and returns how many winning times
-- there are in the big game
part2 :: [(Int, Int)] -> Int
part2 game = part1 [foldl concatenate (0, 0) game]
  where
    concatenate (tAcc, dAcc) (t, d) = (tAcc * scale t + t, dAcc * scale d + d)

-- calculates the "scale" of a number + 1 and returns the magnitude ie. 8 -> 10, 23 -> 100, 694 -> 1000
scale :: Int -> Int
scale n
  | n < 10 = 10
  | otherwise = 10 * scale (n `div` 10)

-- calculates distance travelled in a game based on velocity * time minus "button pressing time"
distance :: Int -> Int -> Int
distance v t = v * (t - v)

-- parses `Time: [n1] [n2] [n3]\nDistance:[n4] [n5] [n6]` and returns `[(n1, n4), (n2, n5), (n3, n6)]`
gameParser :: Parser [(Int, Int)]
gameParser = do
  time <- map read <$> (string "Time:" *> spaces *> many1 digit `endBy` spaces)
  distance <- map read <$> (string "Distance:" *> spaces *> many1 digit `endBy` spaces)
  return $ zip time distance
