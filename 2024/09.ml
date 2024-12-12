#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

let input =
  let rec calc blocks n xs =
    match xs with
    | [] ->
        blocks
    | block_size :: free_space :: xs ->
        calc (blocks @ [(Some n, block_size); (None, free_space)]) (n + 1) xs
    | block_size :: xs ->
        calc (blocks @ [(Some n, block_size)]) (n + 1) xs
  in
  read_line () |> String.to_seq
  |> Seq.map (String.make 1)
  |> Seq.map int_of_string |> List.of_seq |> calc [] 0

let rec remove_last lst =
  let rec aux = function
    | [] ->
        (None, [])
    | (Some block_id, size) :: xs ->
        (Some (block_id, size), List.rev xs)
    | (None, _) :: xs ->
        aux xs
  in
  List.rev lst |> aux

let rec replace_at n replacement = function
  | [] ->
      []
  | x :: xs ->
      if n == 0 then replacement @ xs
      else x :: replace_at (n - 1) replacement xs

let rec fold_res n acc lst =
  match lst with
  | [] ->
      acc
  | (_, 0) :: xs ->
      fold_res n acc xs
  | (None, size) :: xs ->
      fold_res (n + size) acc xs
  | (Some block_id, size) :: xs ->
      fold_res (n + 1) ((block_id * n) + acc) ((Some block_id, size - 1) :: xs)

let part1 =
  let rec reorg curr input =
    let handle free_space block_id size xs =
      if free_space > size then
        (Some block_id, size) :: reorg None ((None, free_space - size) :: xs)
      else if free_space == size then (Some block_id, size) :: reorg None xs
      else
        (Some block_id, free_space)
        :: reorg (Some (block_id, size - free_space)) xs
    in
    match (input, curr) with
    | [], None ->
        []
    | [], Some (block_id, size) ->
        [(Some block_id, size)]
    | (Some block_id, size) :: xs, _ ->
        (Some block_id, size) :: reorg curr xs
    | (None, free_space) :: xs, Some (block_id, size) ->
        handle free_space block_id size xs
    | (None, free_space) :: xs, None -> (
      match remove_last xs with
      | Some (block_id, size), xs ->
          handle free_space block_id size xs
      | None, xs ->
          reorg curr xs )
  in
  input |> reorg None |> fold_res 0 0

let part2 =
  let rec find_fitting_free_space required_space to_scan n =
    match to_scan with
    | [] ->
        None
    | (None, block_size) :: xs ->
        if block_size >= required_space then Some (n, block_size)
        else find_fitting_free_space required_space xs (n + 1)
    | (Some _, _) :: xs ->
        find_fitting_free_space required_space xs (n + 1)
  in
  let rec reorg input =
    match input with
    | [] ->
        []
    | (None, size) :: xs ->
        (None, size) :: reorg xs
    | (Some block_id, size) :: xs -> (
      match find_fitting_free_space size (List.rev xs) 0 with
      | None ->
          (Some block_id, size) :: reorg xs
      | Some (idx, space) ->
          (None, size)
          :: replace_at
               (List.length xs - idx - 1)
               [(None, space - size); (Some block_id, size)]
               xs
          |> reorg )
  in
  input |> List.rev |> reorg |> List.rev |> fold_res 0 0

let _ = print_int part1

let _ = print_endline ""

let _ = print_int part2

let _ = print_endline ""
