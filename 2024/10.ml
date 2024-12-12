#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

let input =
  let rec aux () =
    try
      let parsed_line =
        read_line () |> String.to_seq
        |> Seq.map (String.make 1)
        |> Seq.map int_of_string |> List.of_seq
      in
      parsed_line :: aux ()
    with End_of_file -> []
  in
  aux ()

let input' = input |> List.map Array.of_list |> Array.of_list

let is_valid (x, y) =
  x >= 0 && y >= 0 && y < Array.length input' && x < Array.length input'.(0)

let trailheads =
  let rec aux x y = function
    | [] ->
        []
    | [] :: rows ->
        aux 0 (y + 1) rows
    | (0 :: cols) :: rows ->
        (x, y) :: aux (x + 1) y (cols :: rows)
    | (_ :: cols) :: rows ->
        aux (x + 1) y (cols :: rows)
  in
  aux 0 0 input

module RatingMap = Map.Make (struct
  type t = int * int

  let compare = compare
end)

let ratings =
  let rec find_paths curr (x, y) =
    if curr = 9 then RatingMap.singleton (x, y) 1
    else
      let next = curr + 1 in
      [(x, y - 1); (x, y + 1); (x - 1, y); (x + 1, y)]
      |> List.filter is_valid
      |> List.filter (fun (x, y) -> input'.(y).(x) = next)
      |> List.map (find_paths next)
      |> List.fold_left
           (RatingMap.union (fun _ a b -> Some (a + b)))
           RatingMap.empty
  in
  List.map (find_paths 0) trailheads

let part1 =
  List.fold_left (fun acc score -> acc + RatingMap.cardinal score) 0 ratings

let _ = print_int part1

let _ = print_newline ()

let part2 =
  List.fold_left
    (fun acc score ->
      acc + RatingMap.fold (fun _ value acc -> value + acc) score 0 )
    0 ratings

let _ = print_int part2

let _ = print_newline ()
