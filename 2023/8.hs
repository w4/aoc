#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Data.List
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

{- https://adventofcode.com/2023/day/8 -}

main = do
  document <- readAndParseStdin
  print $ part1 document
  print $ part2 document

-- find the path from AAAA -> ZZZZ
part1 :: Document -> Int
part1 doc = traverseTreeUntilD doc (== "ZZZ") "AAA"

-- find the length of each startingPosition loop, find least common multiple between all
part2 :: Document -> Int
part2 doc = foldl1 lcm . map (traverseTreeUntilD doc ("Z" `isSuffixOf`)) $ startingPositions
  where
    startingPositions = filter ("A" `isSuffixOf`) $ (Map.keys . docMap) doc

-- helper function to call traverseTreeUntil with a document
traverseTreeUntilD :: Document -> (String -> Bool) -> String -> Int
traverseTreeUntilD doc = traverseTreeUntil (cycle $ directions doc) (docMap doc) 0

-- traverse the tree until the predicate is matched
traverseTreeUntil :: [Direction] -> Map.Map String (String, String) -> Int -> (String -> Bool) -> String -> Int
traverseTreeUntil (x : xs) m n predicate elem
  | predicate elem = n
  | otherwise = traverseTreeUntil xs m (n + 1) predicate (readNext getNode x)
  where
    getNode = case Map.lookup elem m of
      Just a -> a
      Nothing -> error (show elem)
    readNext (n, _) DirectionLeft = n
    readNext (_, n) DirectionRight = n

-- parse entirety of stdin
readAndParseStdin :: IO Document
readAndParseStdin = do
  content <- getContents
  case parse parseDocument "" content of
    Left parseError -> error $ show parseError
    Right doc -> return doc

-- parse entire document
parseDocument :: Parser Document
parseDocument = do
  directions <- many1 parseDirection
  _ <- string "\n\n"
  nodes <- parseNode `endBy` char '\n'
  return
    Document
      { directions,
        docMap = Map.fromList nodes
      }

-- parse a single node `AAA = (BBB, CCC)`
parseNode :: Parser (String, (String, String))
parseNode = do
  node <- many1 alphaNum
  _ <- spaces <* char '=' <* spaces
  _ <- char '('
  left <- many1 alphaNum
  _ <- char ',' <* spaces
  right <- many1 alphaNum
  _ <- char ')'
  return (node, (left, right))

-- parse the direction string `LRLLLR`
parseDirection :: Parser Direction
parseDirection = do
  c <- oneOf "LR"
  return $ case c of
    'L' -> DirectionLeft
    'R' -> DirectionRight

data Document = Document
  { directions :: [Direction],
    docMap :: Map.Map String (String, String)
  }
  deriving (Show)

data Direction = DirectionLeft | DirectionRight deriving (Enum, Show)
