#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ordered-containers ])"

import Aoc (readAndParseStdin)
import Data.Char (ord)
import qualified Data.Map as Map
import qualified Data.Map.Ordered as OM
import Text.Parsec (char, choice, digit, many, many1, noneOf, parse, sepBy)
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parseInput
  print $ part1 input
  print $ part2 input

part1 :: [String] -> Int
part1 input = sum $ map hash input

part2 :: [String] -> Int
part2 input = calcScore $ foldl folder (Map.fromList (zip [0 ..] $ replicate 256 OM.empty)) input
  where
    calcScore :: Map.Map Int (OM.OMap String Int) -> Int
    calcScore = Map.foldrWithKey (\i v acc -> acc + ((i + 1) * sum (zipWith (*) [1 ..] (map snd $ OM.assocs v)))) 0
    folder :: Map.Map Int (OM.OMap String Int) -> String -> Map.Map Int (OM.OMap String Int)
    folder acc input =
      let (label, add, focalLength) = parsePart2Input input
          hashedLabel = hash label
          updateBox = OM.alter (\_ -> if add then Just focalLength else Nothing) label
       in Map.adjust updateBox hashedLabel acc

hash :: String -> Int
hash = foldl (\acc c -> ((acc + ord c) * 17) `rem` 256) 0

parsePart2Input :: String -> (String, Bool, Int)
parsePart2Input input = case parse doParse "" input of
  Left parseError -> error $ show parseError
  Right doc -> doc
  where
    doParse = do
      label <- many1 (noneOf "-=")
      operation <-
        choice
          [ True <$ char '=',
            False <$ char '-'
          ]
      focalLength <- read <$> many digit
      return (label, operation, focalLength)

parseInput :: Parser [String]
parseInput = many1 (noneOf ",\n") `sepBy` char ','
