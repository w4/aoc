#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (readAndParseStdin)
import Data.Bifunctor (Bifunctor (first))
import Data.Map (Map, adjust, elems, (!))
import qualified Data.Map as Map
import Data.Set (Set, intersection)
import qualified Data.Set as Set
import Text.Parsec (between, char, choice, digit, endBy, endBy1, getInput, letter, many1, optionMaybe, sepBy, sepBy1, string, try)
import Text.Parsec.Char (noneOf)
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parse
  print $ part1 input
  print $ part2 input

part1 :: Game -> Int
part1 input = sum $ map (sum . elems) $ filter (evaluateInput "in" (workflows input)) (inputs input)

part2 :: Game -> Int
part2 input = sum $ map (product . fst) $ filter ((== Accept) . snd) $ map (first $ elems . Map.map Set.size) $ calculateAllowedBounds $ findAllCompletionBounds "in" $ workflows input

-- Given a list of conditions, calculate all the allowed values for each of x, m, a & s
calculateAllowedBounds :: [([Cond], Action)] -> [(Map ItemKey (Set Int), Action)]
calculateAllowedBounds =
  let rangeSet = Set.fromList [1 .. 4000]
      initAcc = Map.fromList [(X, rangeSet), (M, rangeSet), (A, rangeSet), (S, rangeSet)]
   in map (first $ calcSet initAcc)
  where
    buildRange :: Cond -> Set Int
    buildRange cond = case cond of
      Cond (_, Lt, v) -> Set.fromList [1 .. v - 1]
      Cond (_, Gt, v) -> Set.fromList [v + 1 .. 4000]
    calcSet :: Map ItemKey (Set Int) -> [Cond] -> Map ItemKey (Set Int)
    calcSet acc [] = acc
    calcSet acc (x : xs) =
      let Cond (k, _, _) = x
       in calcSet (adjust (`intersection` buildRange x) k acc) xs

-- Traverse each workflow branch until a "completion" is found and return all the conditions that led up to
-- that particular action
findAllCompletionBounds :: String -> Map String [(Maybe Cond, Action)] -> [([Cond], Action)]
findAllCompletionBounds entryPoint workflows = recurse [] (workflows ! entryPoint)
  where
    recurse :: [Cond] -> [(Maybe Cond, Action)] -> [([Cond], Action)]
    recurse cacc [] = []
    recurse cacc ((cond, action) : xs) =
      let nextCacc = case cond of
            Just c -> c : cacc
            Nothing -> cacc
          inverseCacc = case cond of
            Just c -> invertCond c : cacc
            Nothing -> cacc
       in case action of
            Reject -> (nextCacc, Reject) : recurse inverseCacc xs
            Accept -> (nextCacc, Accept) : recurse inverseCacc xs
            Redirect s -> recurse nextCacc (workflows ! s) ++ recurse inverseCacc xs
      where
        invertCond (Cond (k, Lt, v)) = Cond (k, Gt, v - 1)
        invertCond (Cond (k, Gt, v)) = Cond (k, Lt, v + 1)

-- Calculate whether the given input is allowed by the workflow
evaluateInput :: String -> Map String [(Maybe Cond, Action)] -> Map ItemKey Int -> Bool
evaluateInput entryPoint workflows input = go entryPoint
  where
    go workflow = case evaluateSingleWorkflow (workflows ! workflow) input of
      Accept -> True
      Reject -> False
      Redirect s -> go s
    evaluateSingleWorkflow workflow input = go workflow
      where
        go [] = error "no fallback"
        go ((cond, action) : xs) = case cond of
          Nothing -> action
          Just (Cond (k, Lt, v)) -> if (input ! k) < v then action else go xs
          Just (Cond (k, Gt, v)) -> if (input ! k) > v then action else go xs

parse :: Parser Game
parse = do
  workflows <- Map.fromList <$> parseWorkflow `endBy1` char '\n'
  _ <- char '\n'
  inputs <- (Map.fromList <$> parseInput) `sepBy` char '\n'
  return
    Game
      { workflows,
        inputs
      }

parseInput :: Parser [(ItemKey, Int)]
parseInput = braced $ kvPair `sepBy` char ','
  where
    kvPair = do
      key <- parseItemKey <* char '='
      value <- read <$> many1 digit
      return (key, value)

braced = between (char '{') (char '}')

parseWorkflow :: Parser (String, [(Maybe Cond, Action)])
parseWorkflow = do
  key <- many1 (noneOf "{\n")
  conds <- braced $ parseWorkflowItem `sepBy` char ','
  return (key, conds)

parseWorkflowItem :: Parser (Maybe Cond, Action)
parseWorkflowItem = do
  cond <- optionMaybe (try parseCond <* char ':')
  action <-
    choice
      [ Accept <$ char 'A',
        Reject <$ char 'R',
        Redirect <$> many1 letter
      ]
  return (cond, action)

parseCond :: Parser Cond
parseCond = do
  item <- parseItemKey
  cond <-
    choice
      [ Lt <$ char '<',
        Gt <$ char '>'
      ]
  val <- read <$> many1 digit
  return $ Cond (item, cond, val)

parseItemKey :: Parser ItemKey
parseItemKey =
  choice
    [ X <$ char 'x',
      M <$ char 'm',
      A <$ char 'a',
      S <$ char 's'
    ]

data Game = Game
  { workflows :: Map String [(Maybe Cond, Action)],
    inputs :: [Map ItemKey Int]
  }
  deriving (Show)

newtype Cond = Cond (ItemKey, CondOp, Int) deriving (Show)

data CondOp = Lt | Gt deriving (Enum, Show)

data Action = Accept | Reject | Redirect String deriving (Show, Eq)

data ItemKey = X | M | A | S deriving (Enum, Show, Ord, Eq)
