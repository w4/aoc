#!/usr/bin/env nix-shell

(*
#!nix-shell --pure -i ocaml -p ocaml
*)

module RegisterMap = Map.Make (Char)

type input = {registers: int RegisterMap.t; program: int list}

let input =
  let initial_state = {registers= RegisterMap.empty; program= []} in
  let parse_register input state =
    match String.split_on_char ' ' input with
    | ["Register"; register; value] ->
        { state with
          registers=
            RegisterMap.add register.[0] (int_of_string value) state.registers
        }
    | _ ->
        failwith (Printf.sprintf "bad registers: %s" input)
  in
  let parse_program input state =
    match String.split_on_char ' ' input with
    | ["Program:"; program] ->
        { state with
          program= program |> String.split_on_char ',' |> List.map int_of_string
        }
    | _ ->
        failwith (Printf.sprintf "bad program: %s" input)
  in
  let rec read_input reading_registers state =
    try
      match (read_line (), reading_registers) with
      | "", _ ->
          read_input false state
      | v, true ->
          parse_register v state |> read_input true
      | v, false ->
          parse_program v state |> read_input false
    with End_of_file -> state
  in
  read_input true initial_state

type instruction = ADV | BXL | BST | JNZ | BXC | OUT | BDV | CDV

let instruction_of = function
  | 0 ->
      ADV
  | 1 ->
      BXL
  | 2 ->
      BST
  | 3 ->
      JNZ
  | 4 ->
      BXC
  | 5 ->
      OUT
  | 6 ->
      BDV
  | 7 ->
      CDV
  | _ ->
      failwith "invalid instruction"

let read_register registers = function
  | (0 | 1 | 2 | 3) as x ->
      x
  | 4 ->
      RegisterMap.find 'A' registers
  | 5 ->
      RegisterMap.find 'B' registers
  | 6 ->
      RegisterMap.find 'C' registers
  | _ ->
      failwith "bad register"

let rec pow2 exp = if exp = 0 then 1 else 2 * pow2 (exp - 1)

let take_after n arr =
  let rec aux n arr =
    match (n, arr) with
    | 0, x :: xs ->
        x :: aux 0 xs
    | n, x :: xs ->
        aux (n - 1) xs
    | _, [] ->
        []
  in
  aux n arr

let rec run_program program registers =
  let execute instr operand xs =
    match instruction_of instr with
    | (ADV | BDV | CDV) as instr ->
        let numerator, raw_denominator =
          (RegisterMap.find 'A' registers, read_register registers operand)
        in
        let output_register =
          match instr with
          | ADV ->
              'A'
          | BDV ->
              'B'
          | CDV ->
              'C'
          | _ ->
              failwith "unreachable"
        in
        RegisterMap.add output_register
          (numerator / pow2 raw_denominator)
          registers
        |> run_program xs
    | BXL ->
        let base = RegisterMap.find 'B' registers in
        RegisterMap.add 'B' (base lxor operand) registers |> run_program xs
    | BST ->
        let base = read_register registers operand in
        RegisterMap.add 'B' (base land 0b111) registers |> run_program xs
    | JNZ ->
        let xs =
          if RegisterMap.find 'A' registers = 0 then xs
          else take_after operand input.program
        in
        run_program xs registers
    | BXC ->
        let b, c =
          (RegisterMap.find 'B' registers, RegisterMap.find 'C' registers)
        in
        RegisterMap.add 'B' (b lxor c) registers |> run_program xs
    | OUT ->
        let ret = read_register registers operand |> ( land ) 0b111 in
        Some (ret, xs, registers)
  in
  match program with
  | instr :: operand :: xs ->
      execute instr operand xs
  | instr :: xs ->
      failwith "bad opcodes"
  | [] ->
      None

let part1 =
  let rec aux program registers =
    match run_program program registers with
    | Some (ret, program, registers) ->
        ret :: aux program registers
    | None ->
        []
  in
  aux input.program input.registers
  |> List.map string_of_int |> String.concat "," |> print_endline

let part2 =
  let build_register value = RegisterMap.add 'A' value input.registers in
  let check_value expected_output value =
    match run_program input.program (build_register value) with
    | Some (value, _, _) ->
        value = expected_output
    | None ->
        false
  in
  let try_digit acc x =
    let possible_values = List.init 8 (fun i -> (acc lsl 3) lor i) in
    List.find_opt (check_value x) possible_values
  in
  let rec build_solution acc = function
    | [] ->
        Some acc
    | x :: xs ->
        try_digit acc x |> Fun.flip Option.bind (Fun.flip build_solution xs)
  in
  input.program |> List.rev |> build_solution 0 |> Option.get |> print_int
  |> print_newline
