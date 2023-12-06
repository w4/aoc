#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Control.Applicative ((<*))
import Data.Map (Map, elems, fromListWith)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

{- https://adventofcode.com/2023/day/2 -}

main = do
  input <- getContents
  case parseString input of
    Left err -> print err
    Right games -> do
      part1PrintValidGamesMaxCubes games
      part2PrintMinimumRequiredCubes games

-- print the sum of game ids that can be played with `cubesAllowed` cubes
part1PrintValidGamesMaxCubes :: [Game] -> IO ()
part1PrintValidGamesMaxCubes games = do
  print $ sum $ map gameId (filter checkGameIsValid games)

-- print the sum of the "power" required to play each game (which is just the product of max(amount) per colour)
part2PrintMinimumRequiredCubes :: [Game] -> IO ()
part2PrintMinimumRequiredCubes games = do
  print $ sum $ map (product . elems . getMinimumCubesRequiredForGame) games

-- fold every round in a game into map<string, int> where string is a colour and int is the max cubes for the colour
getMinimumCubesRequiredForGame :: Game -> Map String Int
getMinimumCubesRequiredForGame game = fromListWith max $ concat (rounds game)

-- check if every colourset pulled within a game is within the bounds of `cubesAllowed`
checkGameIsValid :: Game -> Bool
checkGameIsValid game = all (all isCubeAmountAllowed) (rounds game)

-- check if the given colour, amount tuple is within the allowed range
isCubeAmountAllowed :: (String, Int) -> Bool
isCubeAmountAllowed (colour, amount) = amount <= cubesAllowed colour

-- consts set by the task
cubesAllowed "red" = 12
cubesAllowed "green" = 13
cubesAllowed "blue" = 14
cubesAllowed _ = 0

data Game = Game
  { gameId :: Int,
    rounds :: [[(String, Int)]]
  }
  deriving (Show)

-- parse `Game [n]: [n] [colour], [n] [colour], ...; [n] [colour]; Game [n]...`
parseString :: String -> Either ParseError [Game]
parseString = parse fullParser ""

fullParser :: Parser [Game]
fullParser = gameParser `sepBy` char '\n'

-- parse a single game
gameParser :: Parser Game
gameParser = do
  _ <- string "Game "
  gameId <- many1 digit <* char ':' <* spaces
  rounds <- roundParser `sepBy` (char ';' <* spaces)

  return
    Game
      { gameId = read gameId,
        rounds
      }

-- parse all the colour, count tuples in a given round
roundParser :: Parser [(String, Int)]
roundParser = cubeNumberParser `sepBy` (char ',' <* spaces)

-- parse a single colour, count tuple
cubeNumberParser :: Parser (String, Int)
cubeNumberParser = do
  amount <- many1 digit <* spaces
  colour <- many1 letter
  return (colour, read amount)
