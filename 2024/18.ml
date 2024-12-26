#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

let blocks =
  let rec aux () =
    try
      match read_line () |> String.split_on_char ',' with
      | [x; y] ->
          (int_of_string x, int_of_string y) :: aux ()
      | _ ->
          failwith "bad input"
    with End_of_file -> []
  in
  aux ()

let rec take n xs =
  match (n, xs) with 0, _ | _, [] -> [] | _, x :: xs -> x :: take (n - 1) xs

module PosMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let traverse blocks =
  let grid_size = (70, 70) in
  let start_pos = (0, 0) in
  let target_pos = grid_size in
  let is_valid_position (x, y) =
    x >= 0 && y >= 0
    && x <= fst grid_size
    && y <= snd grid_size
    && not (List.mem (x, y) blocks)
  in
  let rec bfs queue distances =
    match queue with
    | [] ->
        None
    | (pos, dist) :: rest ->
        if pos = target_pos then Some dist
        else
          let neighbours =
            [(0, -1); (0, 1); (1, 0); (-1, 0)]
            |> List.map (fun (dx, dy) -> (fst pos + dx, snd pos + dy))
            |> List.filter is_valid_position
            |> List.filter (fun p -> not (PosMap.mem p distances))
          in
          let new_distances =
            List.fold_left
              (fun acc p -> PosMap.add p (dist + 1) acc)
              distances neighbours
          in
          let new_queue = rest @ List.map (fun p -> (p, dist + 1)) neighbours in
          bfs new_queue new_distances
  in
  bfs [(start_pos, 0)] (PosMap.singleton start_pos 0)

let part1 =
  traverse (take 1024 blocks) |> Option.get |> print_int |> print_newline

let part2 =
  let first_allowed_from_back =
    Array.init (List.length blocks) Fun.id
    |> Array.to_list
    |> List.find (fun n ->
           traverse (take (List.length blocks - n) blocks) |> Option.is_some )
  in
  let x, y = List.nth blocks (List.length blocks - first_allowed_from_back) in
  Printf.printf "%d,%d\n" x y
