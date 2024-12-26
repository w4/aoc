#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

let grid_size = (101, 103)

let midpoint = (fst grid_size / 2, snd grid_size / 2)

type robot = {p: int * int; v: int * int}

let input : robot list =
  let parse_coord coord =
    match
      String.sub coord 2 (String.length coord - 2) |> String.split_on_char ','
    with
    | [x; y] ->
        (int_of_string x, int_of_string y)
    | _ ->
        failwith "invalid coord"
  in
  let parse_line line =
    match String.split_on_char ' ' line with
    | [pos; vel] ->
        {p= parse_coord pos; v= parse_coord vel}
    | _ ->
        failwith "invalid line format"
  in
  let rec read_input acc =
    try (read_line () |> parse_line) :: acc |> read_input
    with End_of_file -> acc
  in
  read_input []

let interpolate n =
  let positive_mod x m = ((x mod m) + m) mod m in
  let interpolate_single robot : robot =
    { p=
        ( positive_mod (fst robot.p + (fst robot.v * n)) (fst grid_size)
        , positive_mod (snd robot.p + (snd robot.v * n)) (snd grid_size) )
    ; v= robot.v }
  in
  input |> List.map interpolate_single

let find_in_segments robots =
  let in_segment robot =
    if fst robot.p < fst midpoint && snd robot.p < snd midpoint then
      Some 0 (* top left *)
    else if fst robot.p > fst midpoint && snd robot.p < snd midpoint then
      Some 1 (* top right *)
    else if fst robot.p < fst midpoint && snd robot.p > snd midpoint then
      Some 2 (* bottom left *)
    else if fst robot.p > fst midpoint && snd robot.p > snd midpoint then
      Some 3 (* bottom right *)
    else None (* middle *)
  in
  robots |> List.filter_map in_segment

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

let group_and_mul lst =
  let aux acc x =
    let count = (IntMap.find_opt x acc |> Option.value ~default:0) + 1 in
    IntMap.add x count acc
  in
  let fold _key value acc = acc * value in
  let grouped = lst |> List.fold_left aux IntMap.empty in
  IntMap.fold fold grouped 1

let rec debug = function
  | x :: xs ->
      print_int x |> print_newline ;
      debug xs
  | [] ->
      ()

let rec debug' robots =
  let print_robot robot =
    print_string "x=" ;
    print_int (fst robot.p) ;
    print_string " y=" ;
    print_int (snd robot.p) |> print_newline
  in
  match robots with x :: xs -> print_robot x ; debug' xs | [] -> ()

let part1 =
  interpolate 100 |> find_in_segments |> group_and_mul |> print_int
  |> print_newline

let part2 =
  (* check if all the robots are on unique squares *)
  let rec heuristic seen xs =
    match xs with
    | [] ->
        true
    | x :: xs ->
        if List.mem x seen then false else heuristic (x :: seen) xs
  in
  let rec debug x y curr =
    if x > fst grid_size then (
      print_newline () ;
      debug 0 (y + 1) curr )
    else if y > snd grid_size then print_newline ()
    else if List.mem (x, y) curr then (
      print_int 1 ;
      debug (x + 1) y curr )
    else (
      print_char '.' ;
      debug (x + 1) y curr )
  in
  let rec aux n =
    let curr = interpolate n |> List.map (fun robot -> robot.p) in
    if heuristic [] curr then (
      print_int n |> print_newline ;
      debug 0 0 curr )
    else aux (n + 1)
  in
  aux 0
