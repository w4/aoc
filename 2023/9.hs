#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Control.Monad (guard)
import Data.List (unfoldr)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)
import Aoc (readAndParseStdin)

main = do
  input <- readAndParseStdin parseInput
  print $ part1 input
  print $ part2 input

-- interpolate the next value on every input and sum them
part1 :: [[Int]] -> Int
part1 = sum . map (round . interpolateNext)

-- interpolate the previous value on every input and sum them
part2 :: [[Int]] -> Int
part2 = sum . map (round . interpolatePrevious)

-- helper function to call interpolatePolynomial for the next value
interpolateNext :: [Int] -> Double
interpolateNext i = interpolatePolynomial (length i) i

-- helper function to call interpolatePolynomial for the previous value
interpolatePrevious :: [Int] -> Double
interpolatePrevious = interpolatePolynomial (-1)

-- given an nth term and a sequence, calculate newton's polynomial and
-- interpolate the nth value for the sequence
interpolatePolynomial :: Int -> [Int] -> Double
interpolatePolynomial nth seq =
  let divDiff = (dividedDifference . buildDifferenceTable) seq
      initialValue = (1, 0)
      (_, val) = foldl foldFunction initialValue $ zip [0 ..] (tail divDiff)
   in head divDiff + val
  where
    foldFunction (productAcc, valueAcc) (idx, val) =
      let prod = productAcc * fromIntegral (nth - idx)
       in (prod, valueAcc + (val * prod))

-- calculate the divided differences from our table for newton's
-- polynomial
dividedDifference :: [[Int]] -> [Double]
dividedDifference table = [fromIntegral (head row) / fromIntegral (fac i) | (i, row) <- zip [0 ..] table]
  where
    fac i = product [1 .. i]

-- build the difference table of each input
buildDifferenceTable :: [Int] -> [[Int]]
buildDifferenceTable input = input : unfoldr buildRow input
  where
    zipPairs list = zip list $ tail list
    diffPairs = map $ uncurry subtract
    buildRow lst =
      let row = diffPairs $ zipPairs lst
       in guard (not $ null row) >> Just (row, row)

-- parse each input line
parseInput :: Parser [[Int]]
parseInput = parseSequence `sepBy` char '\n'

-- parse sequence of numbers
parseSequence :: Parser [Int]
parseSequence = map read <$> many1 (digit <|> char '-') `sepBy` char ' '
