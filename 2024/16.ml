#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

module CoordSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

type direction = North | South | East | West

module CoordMap = Map.Make (struct
  type t = (int * int) * direction

  let compare = compare
end)

let walls, start_point, end_point =
  let with_wall wall (walls, start_point, end_point) =
    (CoordSet.add wall walls, start_point, end_point)
  in
  let with_start_point start_point (walls, _, end_point) =
    (walls, Some start_point, end_point)
  in
  let with_end_point end_point (walls, start_point, _) =
    (walls, start_point, Some end_point)
  in
  let rec parse_line x y = function
    | '#' :: xs ->
        parse_line (x + 1) y xs |> with_wall (x, y)
    | 'E' :: xs ->
        parse_line (x + 1) y xs |> with_end_point (x, y)
    | 'S' :: xs ->
        parse_line (x + 1) y xs |> with_start_point (x, y)
    | '.' :: xs ->
        parse_line (x + 1) y xs
    | x :: xs ->
        failwith (Printf.sprintf "invalid character in input %c" x)
    | [] ->
        (CoordSet.empty, None, None)
  in
  let rec aux walls start_point end_point y =
    try
      let new_walls, new_start_point, new_end_point =
        read_line () |> String.to_seq |> List.of_seq |> parse_line 0 y
      in
      let start_point =
        match start_point with Some _ as x -> x | None -> new_start_point
      in
      let end_point =
        match end_point with Some _ as x -> x | None -> new_end_point
      in
      aux (CoordSet.union walls new_walls) start_point end_point (y + 1)
    with End_of_file -> (walls, Option.get start_point, Option.get end_point)
  in
  aux CoordSet.empty None None 0

let next_coords (x, y) = function
  | North ->
      (x, y - 1)
  | South ->
      (x, y + 1)
  | West ->
      (x - 1, y)
  | East ->
      (x + 1, y)

let invert_direction = function
  | North ->
      South
  | South ->
      North
  | East ->
      West
  | West ->
      East

let directions = [North; South; East; West]

type state = {dist: int CoordMap.t; queue: (int * (int * int) * direction) list}

let traverse start_point origins =
  let initial_state =
    { dist=
        List.fold_left
          (fun acc origin -> CoordMap.add (start_point, origin) 0 acc)
          CoordMap.empty origins
    ; queue= List.map (fun origin -> (0, start_point, origin)) origins }
  in
  let is_better_path dist (x, y) dir score =
    match CoordMap.find_opt ((x, y), dir) dist with
    | Some v ->
        v > score
    | None ->
        true
  in
  let add_to_queue queue entry = List.sort compare (entry :: queue) in
  let process_direction state (score, (x, y), dir) next_dir =
    let new_score = score + 1000 in
    if not (is_better_path state.dist (x, y) next_dir new_score) then state
    else
      { dist= CoordMap.add ((x, y), next_dir) new_score state.dist
      ; queue= add_to_queue state.queue (new_score, (x, y), next_dir) }
  in
  let process_forward state (score, (x, y), dir) =
    let next_pos = next_coords (x, y) dir in
    let new_score = score + 1 in
    if
      CoordSet.mem next_pos walls
      || not (is_better_path state.dist next_pos dir new_score)
    then state
    else
      { dist= CoordMap.add (next_pos, dir) new_score state.dist
      ; queue= add_to_queue state.queue (new_score, next_pos, dir) }
  in
  let process_node state ((score, pos, dir) as node) =
    if is_better_path state.dist pos dir score then state
    else
      let state_after_directions =
        List.fold_left
          (fun acc_state next_dir ->
            if next_dir = dir then acc_state
            else process_direction acc_state node next_dir )
          state directions
      in
      process_forward state_after_directions node
  in
  let rec process_queue state =
    match state.queue with
    | [] ->
        state.dist
    | node :: rest ->
        let state' = {state with queue= rest} in
        process_queue (process_node state' node)
  in
  process_queue initial_state

let part1 =
  traverse start_point [East]
  |> CoordMap.to_list
  |> List.filter (fun ((pos, _), _) -> pos = end_point)
  |> List.map snd |> List.fold_left min max_int

let _ = part1 |> print_int |> print_newline

let part2 =
  let zip o1 o2 =
    match (o1, o2) with Some v1, Some v2 -> Some (v1, v2) | _ -> None
  in
  let from_start = traverse start_point [East] in
  let from_end = traverse end_point directions in
  let max_x =
    walls |> CoordSet.to_list |> List.map fst |> List.fold_left max min_int
  in
  let max_y =
    walls |> CoordSet.to_list |> List.map snd |> List.fold_left max min_int
  in
  let coordinates =
    List.init (max_y + 1) (fun y -> List.init (max_x + 1) (fun x -> (x, y)))
    |> List.concat
  in
  let is_valid_point (x, y) =
    directions
    |> List.filter_map (fun dir ->
           zip
             (CoordMap.find_opt ((x, y), dir) from_start)
             (CoordMap.find_opt ((x, y), invert_direction dir) from_end) )
    |> List.exists (fun (a, b) -> a + b = part1)
  in
  coordinates |> List.filter is_valid_point
  |> List.fold_left (fun acc coord -> CoordSet.add coord acc) CoordSet.empty
  |> CoordSet.cardinal |> print_int |> print_newline
