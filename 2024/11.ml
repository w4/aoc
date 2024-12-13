#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

let input = read_line () |> String.split_on_char ' ' |> List.map int_of_string

let rec pow a = function
  | 0 ->
      1
  | 1 ->
      a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let transform = function
  | 0 ->
      [1]
  | n ->
      let log10 = Float.(to_int (log10 (of_int n))) + 1 in
      let exp = pow 10 (log10 / 2) in
      let lhs = n / exp in
      let rhs = n - (lhs * exp) in
      if log10 mod 2 = 0 then [lhs; rhs] else [n * 2024]

let count n x =
  let memo = Hashtbl.create 100 in
  let rec aux n x =
    match (n, Hashtbl.find_opt memo (n, x)) with
    | _, Some v ->
        v
    | 0, _ ->
        1
    | n, _ ->
        let res =
          transform x |> List.map (aux (n - 1)) |> List.fold_left ( + ) 0
        in
        Hashtbl.add memo (n, x) res ;
        res
  in
  aux n x

let iter n = input |> List.map (count n) |> List.fold_left ( + ) 0

let _ = iter 25 |> print_int |> print_newline

let _ = iter 75 |> print_int |> print_newline
