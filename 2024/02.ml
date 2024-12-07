#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

let rec read_input acc =
  try
    read_input
      ( (read_line () |> String.split_on_char ' ' |> List.map int_of_string)
      :: acc )
  with End_of_file -> acc

let rec is_ordered = function
  | [], _ | [_], _ ->
      true
  | x :: y :: rest, None ->
      is_ordered (y :: rest, Some (x > y))
  | x :: y :: rest, Some gt ->
      if (gt && x > y) || ((not gt) && x < y) then
        is_ordered (y :: rest, Some gt)
      else false

let rec adjacent_within_bounds = function
  | [] | [_] ->
      true
  | x :: y :: rest ->
      let diff = Int.abs (y - x) in
      if diff >= 1 && diff <= 3 then adjacent_within_bounds (y :: rest)
      else false

let is_valid x =
  let unordered_pairs = is_ordered (x, None) in
  let unbounded_adjacent = adjacent_within_bounds x in
  unordered_pairs && unbounded_adjacent

let part1 input = List.filter is_valid input |> List.length

let part2 input =
  let rec remove_at n = function
    | [] ->
        []
    | h :: t ->
        if n = 0 then t else h :: remove_at (n - 1) t
  in
  let rec bruteforce n x =
    if List.length x = n then false
    else if is_valid (remove_at n x) then true
    else bruteforce (n + 1) x
  in
  let is_valid' x = is_valid x || bruteforce 0 x in
  List.filter is_valid' input |> List.length

let input = read_input []

let () = input |> part1 |> print_int

let () = print_endline ""

let () = input |> part2 |> print_int
