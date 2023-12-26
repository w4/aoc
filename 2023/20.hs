#!/usr/bin/env nix-shell
#!nix-shell --pure -i "runghc -- -i../" -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ ])"

import Aoc (readAndParseStdin)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Text.Parsec (char, choice, many, many1, noneOf, sepBy, string)
import Text.Parsec.String (Parser)

main = do
  input <- readAndParseStdin parser
  print $ part1 input
  print $ part2 input

part1 :: Map String Module -> Int
part1 input =
  let (hi, lo) = run input
   in hi * lo
  where
    run :: Map String Module -> (Int, Int)
    run input = go 1000 (mapConfigToState input)
      where
        go 0 _ = (0, 0)
        go i state =
          let (nextState, hi, lo, _) = handlePulse input Nothing state "broadcaster" False
              (nextHi, nextLo) = go (i - 1) nextState
           in (hi + nextHi, lo + nextLo)

part2 :: Map String Module -> Int
part2 input =
  let rxInput = head $ findKeysForDst "rx"
      pInputs = findKeysForDst rxInput
   in foldl1 lcm $ findMinPressesForKey pInputs
  where
    findKeysForDst rx = Map.keys $ Map.filter (elem rx . dst) input
    findMinPressesForKey [x] = [scan x (mapConfigToState input) 2]
    findMinPressesForKey (x : xs) = scan x (mapConfigToState input) 1 : findMinPressesForKey xs
    scan x state i =
      let (nextState, _, _, r) = handlePulse input (Just x) state "broadcaster" False
       in if r then i else scan x nextState (i + 1)

mapConfigToState :: Map String Module -> Map String State
mapConfigToState input = Map.mapWithKey (\k v -> doMap k (kind v)) input
  where
    doMap _ KindBroadcaster = Broadcaster
    doMap _ KindFlipFlop = FlipFlop False
    doMap k KindConjunction = Conjunction $ Map.fromList $ map (,False) $ Map.keys $ Map.filter (\v -> k `elem` dst v) input

handlePulse :: Map String Module -> Maybe String -> Map String State -> String -> Bool -> (Map String State, Int, Int, Bool)
handlePulse config exitOnLow = go "" 0 1 False
  where
    go src hiCnt loCnt hitCond state component pulse =
      let c = config ! component
          nHitCond = hitCond || (Just component == exitOnLow && not pulse)
          publishF pulse (state, hiCnt, loCnt, hitCond) output =
            let nHiCnt = if pulse then hiCnt + 1 else hiCnt
                nLoCnt = if pulse then loCnt else loCnt + 1
             in go component nHiCnt nLoCnt hitCond state output pulse
          publish s pulse = foldl (publishF pulse) (s, hiCnt, loCnt, nHitCond) (dst c)
       in case Map.lookup component state of
            Just Broadcaster -> publish state pulse
            Just (FlipFlop cs) -> if pulse then (state, hiCnt, loCnt, nHitCond) else publish (Map.insert component (FlipFlop (not cs)) state) (not cs)
            Just (Conjunction cs) ->
              let newCs = Map.insert src pulse cs
                  newState = Map.insert component (Conjunction newCs) state
               in publish newState (not $ and $ Map.elems newCs)
            Nothing -> (state, hiCnt, loCnt, nHitCond)

data State = Broadcaster | FlipFlop Bool | Conjunction (Map String Bool) deriving (Show)

parser :: Parser (Map String Module)
parser = Map.fromList <$> parseLine `sepBy` char '\n'
  where
    parseLine = do
      kind <- parseKind
      name <- many (noneOf " ")
      _ <- string " -> "
      dst <- many1 (noneOf ",\n") `sepBy` string ", "
      return (if kind == KindBroadcaster then "broadcaster" else name, Module {kind, dst})
    parseKind =
      choice
        [ KindBroadcaster <$ string "broadcaster",
          KindFlipFlop <$ char '%',
          KindConjunction <$ char '&'
        ]

data Kind = KindBroadcaster | KindFlipFlop | KindConjunction deriving (Enum, Show, Eq)

data Module = Module
  { kind :: Kind,
    dst :: [String]
  }
  deriving (Show)
