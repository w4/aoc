#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

type direction = Up | Down | Left | Right

type input =
  { walls: (int * int) list
  ; boxes: (int * int * int * int) list
  ; robot: int * int
  ; movements: direction list }

let grid, directions =
  let rec read_input (grid, directions) grid_part =
    try
      match (read_line (), grid_part) with
      | "", _ ->
          read_input (grid, directions) true
      | x, false ->
          read_input (grid ^ x ^ "\n", directions) false
      | x, true ->
          read_input (grid, directions ^ x) true
    with End_of_file -> (grid, directions)
  in
  read_input ("", "") false

let parse_input grid =
  let rec parse_grid output x y = function
    | '\n' :: xs ->
        parse_grid output 0 (y + 1) xs
    | '#' :: xs ->
        parse_grid {output with walls= (x, y) :: output.walls} (x + 1) y xs
    | 'O' :: xs ->
        parse_grid
          {output with boxes= (x, y, x, y) :: output.boxes}
          (x + 1) y xs
    | '[' :: ']' :: xs ->
        parse_grid
          {output with boxes= (x, y, x + 1, y) :: output.boxes}
          (x + 2) y xs
    | '@' :: xs ->
        parse_grid {output with robot= (x, y)} (x + 1) y xs
    | '.' :: xs ->
        parse_grid output (x + 1) y xs
    | x :: xs ->
        failwith ("bad char in grid: " ^ String.make 1 x)
    | [] ->
        output
  in
  let rec parse_directions directions = function
    | '^' :: xs ->
        parse_directions (directions @ [Up]) xs
    | 'v' :: xs ->
        parse_directions (directions @ [Down]) xs
    | '<' :: xs ->
        parse_directions (directions @ [Left]) xs
    | '>' :: xs ->
        parse_directions (directions @ [Right]) xs
    | x :: xs ->
        failwith ("bad char in directions: " ^ String.make 1 x)
    | [] ->
        directions
  in
  parse_grid
    { walls= []
    ; boxes= []
    ; robot= (0, 0)
    ; movements=
        directions |> String.to_seq |> List.of_seq |> parse_directions [] }
    0 0
    (String.to_seq grid |> List.of_seq)

let calc_gps (x, y, _, _) = (y * 100) + x

let next_position (x, y) = function
  | Up ->
      (x, y - 1)
  | Down ->
      (x, y + 1)
  | Left ->
      (x - 1, y)
  | Right ->
      (x + 1, y)

let next_position' (x1, y1, x2, y2) direction =
  let x1, y1 = next_position (x1, y1) direction in
  let x2, y2 = next_position (x2, y2) direction in
  (x1, y1, x2, y2)

let check_wall_intersection input =
  List.mem input.robot input.walls
  || List.exists
       (fun (x1, y1, x2, y2) ->
         List.mem (x1, y1) input.walls || List.mem (x2, y2) input.walls )
       input.boxes

let compare_direction direction (ax1, ay1, ax2, ay2) (bx1, by1, bx2, by2) =
  match direction with
  | Up ->
      Int.max by1 by2 - Int.max ay1 ay2
  | Down ->
      Int.min ay1 ay2 - Int.min by1 by2
  | Left ->
      Int.max bx1 bx2 - Int.max ax1 ax2
  | Right ->
      Int.min ax1 ax2 - Int.min bx1 bx2

let has_intersection_with moved (cx1, cy1, cx2, cy2) =
  List.exists
    (fun (x1, y1, x2, y2) ->
      (x1, y1) = (cx1, cy1)
      || (x1, y1) = (cx2, cy2)
      || (x2, y2) = (cx1, cy1)
      || (x2, y2) = (cx2, cy2) )
    moved

let handle_movement input direction =
  let rx, ry = next_position input.robot direction in
  let sorted_boxes = List.sort (compare_direction direction) input.boxes in
  let rec move_boxes acc moved = function
    | x :: xs ->
        if has_intersection_with moved x then
          let next = next_position' x direction in
          move_boxes (next :: acc) (next :: moved) xs
        else move_boxes (x :: acc) moved xs
    | [] ->
        (acc, moved)
  in
  let boxes, moved = move_boxes [] [(rx, ry, rx, ry)] sorted_boxes in
  let final = {input with robot= (rx, ry); boxes} in
  if check_wall_intersection {final with boxes= moved} then input else final

let rec follow_directions input =
  match input.movements with
  | x :: xs ->
      let out = handle_movement {input with movements= xs} x in
      follow_directions out
  | [] ->
      input

let part1 =
  let input = parse_input grid in
  let final = follow_directions input in
  List.map calc_gps final.boxes
  |> List.fold_left ( + ) 0 |> print_int |> print_newline

let part2 =
  let grid =
    let rec aux acc = function
      | '.' :: xs ->
          aux (acc ^ "..") xs
      | '#' :: xs ->
          aux (acc ^ "##") xs
      | '@' :: xs ->
          aux (acc ^ "@.") xs
      | 'O' :: xs ->
          aux (acc ^ "[]") xs
      | '\n' :: xs ->
          aux (acc ^ "\n") xs
      | x :: xs ->
          failwith ("invalid character in input " ^ String.make 1 x)
      | [] ->
          acc
    in
    aux "" (grid |> String.to_seq |> List.of_seq)
  in
  let input = parse_input grid in
  let final = follow_directions input in
  List.map calc_gps final.boxes
  |> List.fold_left ( + ) 0 |> print_int |> print_newline
