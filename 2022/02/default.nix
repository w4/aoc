{ pkgs ? import <nixpkgs> { } }:
with builtins;
let
  inherit (pkgs) lib;
  input = builtins.readFile ./input;
  # cipher for the opponent's hand
  leftCipher = { "A" = "Rock"; "B" = "Paper"; "C" = "Scissors"; };
  # opponent's choice required for a win
  winConditions = { "Rock" = "Scissors"; "Scissors" = "Paper"; "Paper" = "Rock"; };
  # inverse of win connections
  loseConditions = builtins.listToAttrs (map (pair: lib.nameValuePair pair.value pair.name) (lib.attrsToList winConditions));
  # determines the score for the game from both shape and outcome score
  determineScore = game:
    let
      shapeScore = { "Rock" = 1; "Paper" = 2; "Scissors" = 3; };
      outcomeScore = { "L" = 0; "D" = 3; "W" = 6; };
      gameOutcome = if winConditions.${elemAt game 0} == elemAt game 1 then "W" else if elemAt game 0 == elemAt game 1 then "D" else "L";
    in
    outcomeScore.${gameOutcome} + shapeScore.${elemAt game 0};
  # map X to rock, Y to paper and Z to scissors
  splitAndDecipher = x:
    let
      rightCipher = { "X" = "Rock"; "Y" = "Paper"; "Z" = "Scissors"; };
      split = lib.splitString " " x;
      us = rightCipher.${elemAt split 1};
      them = leftCipher.${elemAt split 0};
    in
    [ us them ];
  # map X to a loss, Z to a win and Y to a draw
  splitAndMapToResult = x:
    let
      split = lib.splitString " " x;
      them = leftCipher.${elemAt split 0};
      desiredOutcome = elemAt split 1;
      us = if desiredOutcome == "X" then winConditions.${them} else if desiredOutcome == "Z" then loseConditions.${them} else them;
    in
    [ us them ];
  # split each individual game
  games = lib.splitString "\n" input;
  # plays game with the given mapper and returns the score
  playGame = f: lib.foldl (x: y: x + y) 0 (map determineScore (map f games));
  # plays using part 1 rules with both sides ciphered
  part1 = playGame splitAndDecipher;
  # plays using aprt 2 rules with our side mapped to final outcome
  part2 = playGame splitAndMapToResult;
  # build json output
  out = builtins.toJSON { inherit part1; inherit part2; };
in
pkgs.writeText "out" out
