#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"
import Data.List (find, isInfixOf)
import Data.Maybe (catMaybes)

{- https://adventofcode.com/2023/day/1 -}

main = print =<< run 0

-- recursively read each line from stdin, concatenating first and last digits and folding the result into a sum
run :: Int -> IO Int
run acc = do
  line <- getLine
  if null line
    then return acc
    else do
      let x = concatFirstLastDigitsInString line
      run $ acc + x

-- read first and last digit in a string and concatenate the two together
concatFirstLastDigitsInString :: String -> Int
concatFirstLastDigitsInString s =
  case catMaybes [findDigitFromLeft "" s, findDigitFromRight "" s] of
    [x, y] -> x * 10 + y
    [x] -> x * 11
    _ -> 0

-- find the first digit in the string, searching from the left hand side
findDigitFromLeft :: String -> String -> Maybe Int
findDigitFromLeft acc "" = findDigit acc
findDigitFromLeft acc (x : xs) = case findDigit acc of
  Just v -> Just v
  Nothing -> findDigitFromLeft (acc ++ [x]) xs

-- find the last digit in the string, searching from the right hand side
findDigitFromRight :: String -> String -> Maybe Int
findDigitFromRight acc "" = findDigit acc
findDigitFromRight acc xs = case findDigit acc of
  Just v -> Just v
  Nothing -> findDigitFromRight (last xs : acc) (init xs)

-- finds a digit in either textual or numeric form and returns it as an int
findDigit :: String -> Maybe Int
findDigit s = case find (`isInfixOf` s) digitAsText of
  Just textual -> lookup textual digitMap
  Nothing -> Nothing
  where
    digitMap =
      [ ("eight", 8),
        ("seven", 7),
        ("three", 3),
        ("nine", 9),
        ("four", 4),
        ("five", 5),
        ("two", 2),
        ("one", 1),
        ("six", 6),
        ("1", 1),
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9)
      ]
    digitAsText = map fst digitMap
