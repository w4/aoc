#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (readAndParseStdin, shoelace)
import Text.Parsec (char, choice, count, digit, hexDigit, many1, sepBy, spaces, string)
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parse
  print $ (score . map fst) input
  print $ (score . map snd) input

score :: [(Direction, Int)] -> Int
score input = round $ shoelace (calculateVertices input) + fromIntegral (perimiter input `div` 2) + 1

perimiter :: [(Direction, Int)] -> Int
perimiter = sum . map snd

calculateVertices :: [(Direction, Int)] -> [(Int, Int)]
calculateVertices input = go input 0 0
  where
    go [] hAcc vAcc = []
    go ((d, c) : xs) hAcc vAcc =
      let (newHAcc, newVAcc) = calcAcc d c hAcc vAcc
       in (newHAcc, newVAcc) : go xs newHAcc newVAcc

calcAcc :: Direction -> Int -> Int -> Int -> (Int, Int)
calcAcc U c hAcc vAcc = (hAcc, vAcc - c)
calcAcc D c hAcc vAcc = (hAcc, vAcc + c)
calcAcc L c hAcc vAcc = (hAcc - c, vAcc)
calcAcc R c hAcc vAcc = (hAcc + c, vAcc)

parse :: Parser [((Direction, Int), (Direction, Int))]
parse = parseLine `sepBy` char '\n'

parseLine :: Parser ((Direction, Int), (Direction, Int))
parseLine = do
  p1d <- parseDirection <* spaces
  p1c <- read <$> many1 digit <* spaces
  p2c <- string "(#" *> count 5 hexDigit
  p2d <- parseNumericDirection <* char ')'
  return ((p1d, p1c), (p2d, read ("0x" ++ p2c)))

parseDirection :: Parser Direction
parseDirection =
  choice
    [ U <$ char 'U',
      D <$ char 'D',
      L <$ char 'L',
      R <$ char 'R'
    ]

parseNumericDirection :: Parser Direction
parseNumericDirection =
  choice
    [ U <$ char '3',
      D <$ char '1',
      L <$ char '2',
      R <$ char '0'
    ]

data Direction = U | D | L | R deriving (Show, Enum)
