#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

let input =
  let rec aux () =
    try
      let parsed_line = read_line () |> String.to_seq |> List.of_seq in
      parsed_line :: aux ()
    with End_of_file -> []
  in
  aux () |> List.filter (fun v -> List.length v > 0)

let input' = input |> List.map Array.of_list |> Array.of_list

let is_valid (x, y) =
  x >= 0 && y >= 0 && y < Array.length input' && x < Array.length input'.(0)

let points =
  let rec walk_matching c x y visited =
    if (not (List.mem (x, y) visited)) && is_valid (x, y) && input'.(y).(x) == c
    then
      (x, y) :: visited
      |> walk_matching c (x - 1) y
      |> walk_matching c (x + 1) y
      |> walk_matching c x (y + 1)
      |> walk_matching c x (y - 1)
    else visited
  in
  let rec aux x y visited = function
    | [] ->
        []
    | [] :: rows ->
        aux 0 (y + 1) visited rows
    | (c :: cols) :: rows ->
        if List.mem (x, y) visited then aux (x + 1) y visited (cols :: rows)
        else
          let matching = walk_matching c x y [] in
          matching :: aux (x + 1) y (matching @ visited) (cols :: rows)
  in
  aux 0 0 [] input

let has_neighbour points (x, y) (dx, dy) =
  let neighbour = (x + dx, y + dy) in
  List.exists (( = ) neighbour) points

let fetch_neighbours points (x, y) =
  [(0, 1); (1, 0); (0, -1); (-1, 0)]
  |> List.filter (has_neighbour points (x, y))

let count_shared_edges points =
  List.fold_left
    (fun acc (x, y) -> acc + (fetch_neighbours points (x, y) |> List.length))
    0 points

let count_corners points =
  let is_corner (x, y) ((dy1, dx1), (dy2, dx2)) =
    let left_match = has_neighbour points (x, y) (dx1, dy1) in
    let right_match = has_neighbour points (x, y) (dx2, dy2) in
    let mid_match = has_neighbour points (x, y) (dx1 + dx2, dy1 + dy2) in
    ((not left_match) && not right_match)
    || (left_match && right_match && not mid_match)
  in
  let count_corners (x, y) =
    [((0, -1), (1, 0)); ((1, 0), (0, 1)); ((0, 1), (-1, 0)); ((-1, 0), (0, -1))]
    |> List.filter (is_corner (x, y))
    |> List.length
  in
  List.fold_left (fun acc (x, y) -> acc + count_corners (x, y)) 0 points

let part1 =
  let calc points =
    let area = List.length points in
    let perimeter = (4 * area) - count_shared_edges points in
    area * perimeter
  in
  points |> List.map calc |> List.fold_left ( + ) 0

let _ = part1 |> print_int |> print_newline

let part2 =
  let calc points = List.length points * count_corners points in
  points |> List.map calc |> List.fold_left ( + ) 0

let _ = part2 |> print_int |> print_newline
